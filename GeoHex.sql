/*
* This library "geohex-plpgsql" is a PL/pgSQL porting of "GeoHex"
* originally licensed by @sa2da (http://geogames.net)
* under Creative Commons BY-SA 2.1 Japan License.
*
* geohex-plpgsql is licensed by ASAHI Kosuke
* under Creative Commons BY-SA 2.1 Japan License.
*/

DROP FUNCTION IF EXISTS geohexVersion() CASCADE;
CREATE FUNCTION geohexVersion() RETURNS text AS '
BEGIN
  RETURN ''3.01'';
END;
' LANGUAGE 'plpgsql';

DROP FUNCTION IF EXISTS geohexGetHexCoordsByZone(_geohexZONE, integer) CASCADE;
DROP FUNCTION IF EXISTS geohexGetHexCoordsByZone(_geohexZONE) CASCADE;
DROP FUNCTION IF EXISTS geohexGetZoneByCode(varchar) CASCADE;
DROP FUNCTION IF EXISTS geohexGetZoneByLocation(double precision, double precision) CASCADE;
DROP FUNCTION IF EXISTS geohexGetZoneByLocation(double precision, double precision, integer) CASCADE;
DROP FUNCTION IF EXISTS _geohexXy2locLongitude(double precision, double precision) CASCADE;
DROP FUNCTION IF EXISTS _geohexXy2locLatitude(double precision, double precision) CASCADE;
DROP FUNCTION IF EXISTS _geohexXy2loc(double precision, double precision) CASCADE;
DROP FUNCTION IF EXISTS _geohexLoc2xy(double precision, double precision) CASCADE;
DROP FUNCTION IF EXISTS _geohexCalcHexSize(integer) CASCADE;
DROP FUNCTION IF EXISTS _geohexHK() CASCADE;
DROP FUNCTION IF EXISTS _geohexHDEG() CASCADE;
DROP FUNCTION IF EXISTS _geohexHBASE() CASCADE;
DROP FUNCTION IF EXISTS _geohexHKEY() CASCADE;
DROP TYPE IF EXISTS _geohexZONE CASCADE;
DROP TYPE IF EXISTS _geohexLOC CASCADE;
DROP TYPE IF EXISTS _geohexXY CASCADE;

CREATE TYPE _geohexXY AS (
  x double precision,
  y double precision
);

CREATE TYPE _geohexLOC AS (
  lat double precision,
  lon double precision
);

CREATE TYPE _geohexZONE AS (
  x integer,
  y integer,
  code varchar(17),
  level integer,
  latitude double precision,
  longitude double precision
);

CREATE FUNCTION _geohexHKEY() RETURNS text AS '
BEGIN
  RETURN ''ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'';
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION _geohexHBASE() RETURNS double precision AS '
BEGIN
  RETURN 20037508.34;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION _geohexHDEG() RETURNS double precision AS '
BEGIN
  RETURN 0.5235987755983; /* pi()*(30/180) */
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION _geohexHK() RETURNS double precision AS '
BEGIN
  RETURN 0.57735026918963; /* tan(_geohexHDEG()) */
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION _geohexCalcHexSize(integer) RETURNS double precision AS '
DECLARE
  iLevel ALIAS FOR $1;
BEGIN
  RETURN _geohexHBASE() / power(3, iLevel+1);
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION _geohexLoc2xy(double precision, double precision) RETURNS _geohexXY AS '
DECLARE
  dLon ALIAS FOR $1;
  dLat ALIAS FOR $2;
  dX double precision;
  dY double precision;
  gXY _geohexXY;
BEGIN
  dX := dLon * _geohexHBASE() / 180;
  dY := ln(tan((90 + dLat) * pi() / 360)) / (pi() / 180) * _geohexHBASE() / 180;
  gXY := ROW(dX, dY);
  return gXY;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION _geohexXy2loc(double precision, double precision) RETURNS _geohexLOC AS '
DECLARE
  dX ALIAS FOR $1;
  dY ALIAS FOR $2;
  dLon double precision;
  dLat double precision;
  gLoc _geohexLOC;
BEGIN
  dLon := (dX / _geohexHBASE()) * 180;
  dLat := (dY / _geohexHBASE()) * 180;
  dLat := 180 / pi() * (2 * atan(exp(dLat * pi() / 180)) - pi() / 2);
  gLoc := ROW(dLat, dLon);
  return gLoc;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION _geohexXy2locLatitude(double precision, double precision) RETURNS double precision AS '
DECLARE
  dX ALIAS FOR $1;
  dY ALIAS FOR $2;
  gLoc _geohexLOC;
BEGIN
  gLoc := _geohexXy2loc(dX, dY);
  RETURN (gLoc).lat;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION _geohexXy2locLongitude(double precision, double precision) RETURNS double precision AS '
DECLARE
  dX ALIAS FOR $1;
  dY ALIAS FOR $2;
  gLoc _geohexLOC;
BEGIN
  gLoc := _geohexXy2loc(dX, dY);
  RETURN (gLoc).lon;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION geohexGetZoneByLocation(double precision, double precision, integer) RETURNS _geohexZONE AS '
DECLARE
  dLat ALIAS FOR $1;
  dLon ALIAS FOR $2;
  iLevel ALIAS FOR $3;
  _iLevel integer;
  dHsize double precision;
  gXY _geohexXY;
  gZone _geohexZONE;
  dUnitX double precision;
  dUnitY double precision;
  dHPosX double precision;
  dHPosY double precision;
  dHX0 double precision;
  dHY0 double precision;
  dHXq double precision;
  dHYq double precision;
  dHX double precision;
  dHY double precision;
  dHXY double precision;
  dHLat double precision;
  dHLon double precision;
  gLoc _geohexLOC;
  cHCode varchar(18);
  i integer;
  dHPow double precision;
  dModX double precision;
  dModY double precision;
  aCode3X integer[];
  aCode3Y integer[];
  cH2 varchar(15);
  nH1 numeric;
  dHA1 integer;
  dHA2 integer;
BEGIN
  IF dLat < -90 or dLat > 90 THEN
    RAISE EXCEPTION ''latitude must be between -90 and 90'';
  END IF;

  IF dLon < -180 or dLon > 180 THEN
    RAISE EXCEPTION ''longitude must be between -180 and 180'';
  END IF;

  IF iLevel < 0 or iLevel > 15 THEN
    RAISE EXCEPTION ''level must be between 0 and 15'';
  END IF;
  
  _iLevel := iLevel + 2;
  dHSize := _geohexCalcHexSize(_iLevel);
  gXY := _geohexLoc2xy(dLon, dLat);
  dUnitX := 6 * dHSize;
  dUnitY := 6 * dHSize * _geohexHK();
  dHPosX  := ((gXY).x + (gXY).y / _geohexHK()) / dUnitX;
  dHPosY  := ((gXY).y - _geohexHK() * (gXY).x) / dUnitY;
  dHX0 := floor(dHPosX);
  dHY0 := floor(dHPosY);
  dHXq := dHPosX - dHX0;
  dHYq := dHPosY - dHY0;
  dHX := round(dHPosX);
  dHY := round(dHPosY);
  
  IF dHYq > -dHXq + 1 THEN
    IF (dHYq < 2 * dHXq) and (dHYq > 0.5 * dHXq) THEN
      dHX := dHX0 + 1;
      dHY := dHY0 + 1;
    END IF;
  ELSIF dHYq < -dHXq + 1 THEN
    IF (dHYq > (2 * dHXq) - 1) and (dHYq < (0.5 * dHXq) + 0.5) THEN
      dHX := dHX0;
      dHY := dHY0;
    END IF;
  END IF;
  
  dHLat := (_geohexHK() * dHX * dUnitX + dHY * dUnitY) / 2;
  dHLon := (dHLat - dHY * dUnitY) / _geohexHK();
  
  gLoc := _geohexXy2loc(dHLon, dHLat);
  
  IF _geohexHBASE() - dHLon <= dHSize THEN
    gLoc := ROW((gLoc).lat, 180.0);
    dHXY := dHX;
    dHX := dHY;
    dHY := dHXY;
  END IF;
  
  dModX := dHX;
  dModY := dHY;

  FOR i IN 0.._iLevel LOOP
    dHPow := power(3.0, _iLevel - i);

    IF dModX >= ceil(dHPow / 2) THEN
      aCode3X[i] := 2;
      dModX := dModX - dHPow;
    ELSIF dModX <= -ceil(dHPow/2) THEN
      aCode3X[i] := 0;
      dModX := dModX + dHPow;
    ELSE
      aCode3X[i] := 1;
    END IF;

    IF dModY >= ceil(dHPow / 2) THEN
      aCode3Y[i] := 2;
      dModY := dModY - dHPow;
    ELSIF dModY <= -ceil(dHPow/2) THEN
      aCode3Y[i] := 0;
      dModY := dModY + dHPow;
    ELSE
      aCode3Y[i] := 1;
    END IF;
  END LOOP;

  cHCode := '''';
  FOR i IN 0.._iLevel LOOP
    cHCode := cHCode || ltrim(to_char(aCode3X[i] * 3 + aCode3Y[i], ''9''));
  END LOOP;

  cH2 := substr(cHCode, 4);
  nH1 := to_number(substr(cHCode,1,3), ''999'');
  dHA1 := floor(nH1/30);
  dHA2 := nH1 % 30;
  cHCode := substr(_geohexHKEY(),dHA1+1,1) || substr(_geohexHKEY(),dHA2+1,1) || cH2;
  gZone := ROW(dHX::integer, dHY::integer, cHCode, iLevel, (gLoc).lat, (gLoc).lon);

  
  RETURN gZone;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION geohexGetZoneByLocation(double precision, double precision) RETURNS _geohexZONE AS '
DECLARE
  dLat ALIAS FOR $1;
  dLon ALIAS FOR $2;
  gZone _geohexZONE;
BEGIN
  gZone := geohexGetZoneByLocation(dLat, dLon, 7);
  RETURN gZone;
END;
' LANGUAGE 'plpgsql';


CREATE FUNCTION geohexGetZoneByCode(varchar) RETURNS _geohexZONE AS '
DECLARE
  cCode ALIAS FOR $1;
  gZone _geohexZONE;
  iLevel integer;
  dHSize double precision;
  dUnitX double precision;
  dUnitY double precision;
  dHX double precision;
  dHY double precision;
  dHPow double precision;
  cHDec9 varchar(18);
  iD9Xlen integer;
  i integer;
  aHDecX integer[];
  aHDecY integer[];
  dHLatX double precision;
  dHLatY double precision;
  gHLoc _geohexLOC;
BEGIN
  IF cCode !~  ''^[A-Za-z][A-Za-z][0-9]*$'' THEN
    RAISE EXCEPTION ''%s is notGeoHex Code'', cCode;
  END IF;
  
  iLevel := length(cCode);
  
  dHSize := _geohexCalcHexSize(iLevel);
  dUnitX := 6 * dHSize;
  dUnitY := 6 * dHSize * _geohexHK();
  dHX := 0;
  dHY := 0;
  cHDec9 := ltrim(to_char((strpos(_geohexHKEY(), substr(cCode,1,1))-1)*30 + (strpos(_geohexHKEY(),substr(cCode,2,1))-1),''999'')) || substr(cCode,3);

  IF substr(cHDec9, 1, 1) ~ ''[15]'' and
     substr(cHDec9, 2, 1) ~ ''[^125]'' and
     substr(cHDec9, 3, 1) ~ ''[^125]'' THEN
    IF substr(cHDec9, 1, 1) = ''5'' THEN
      cHDec9 := ''7'' || substr(cHDec9, 2);
    ELSIF substr(cHDec9, 1, 1) = ''1'' THEN
      cHDec9 := ''3'' || substr(cHDec9, 2);
    END IF;
  END IF;

  iD9Xlen := length(cHDec9);

  FOR i IN 0..iLevel-iD9XLen LOOP
    cHDec9 := ''0'' || cHDec9;
    iD9Xlen := iD9Xlen + 1;
  END LOOP;

  FOR i IN 0..length(cHDec9)-1 LOOP
    aHDecX[i] := floor(to_number(substr(cHDec9,i+1,1),''0'')/3);
    aHDecY[i] := ceil(to_number(substr(cHDec9,i+1,1),''0'')%3);
  END LOOP;

  FOR i IN 0..iLevel LOOP
    dHPow := power(3.0, iLevel - i);
    IF aHDecX[i] = 0 THEN
      dHX := dHX - dHPow;
    ELSIF aHDecX[i] = 2 THEN
      dHX := dHX + dHPow;
    END IF;

    IF aHDecY[i] = 0 THEN
      dHY := dHY - dHPow;
    ELSIF aHDecY[i] = 2 THEN
      dHY := dHY + dHPow;
    END IF;
  END LOOP;

  dHLatY := (_geohexHK() * dHX * dUnitX + dHY * dUnitY) / 2;
  dHLatX := (dHLatY - dHY * dUnitY) / _geohexHK();
  gHLoc  := _geohexXy2loc(dHLatX, dHLatY);
  
  IF (gHLoc).lon >= 180 THEN
    gHLoc := ROW((gHLoc).lat, (gHLoc).lon - 360);
    dHX := dHX - power(3.0, iLevel);
    dHY := dHY + power(3.0, iLevel);
  ELSIF (gHLoc).lon <= -180 THEN
    gHLoc := ROW((gHLoc).lat, (gHLoc).lon + 360);
    dHX := dHX + power(3.0, iLevel);
    dHY := dHY - power(3.0, iLevel);
  END IF;

  gZone := ROW(dHX::integer, dHY::integer, cCode, length(cCode)-2, (gHLoc).lat, (gHLoc).lon);
  
  RETURN gZone;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION geohexGetHexCoordsByZone(_geohexZONE) RETURNS GEOMETRY  AS '
DECLARE
  gZone ALIAS FOR $1;
  gXY _geohexXY;
  tPolygon text;
  dHDeg double precision;
  dHSize double precision;
  dHTop double precision;
  dHBtm double precision;
  dHL double precision;
  dHR double precision;
  dHCl double precision;
  dHCr double precision;
  dH double precision;
BEGIN
  gXY := _geohexLoc2xy((gZone).longitude, (gZone).latitude);
  dHDeg := tan(pi() * (60.0 / 180.0));
  dHSize := _geohexCalcHexSize(length((gZone).code));

  dHTop := _geohexXy2locLatitude((gXY).x, (gXY).y + dHDeg * dHSize);
  dHBtm := _geohexXy2locLatitude((gXY).x, (gXY).y - dHDeg * dHSize);

  dHL := _geohexXy2locLongitude((gXY).x - 2 * dHSize, (gXY).y);
  dHR := _geohexXy2locLongitude((gXY).x + 2 * dHSize, (gXY).y);
  dHCl := _geohexXy2locLongitude((gXY).x - 1 * dHSize, (gXY).y);
  dHCr := _geohexXy2locLongitude((gXY).x + 1 * dHSize, (gXY).y);
  
  tPolygon := ''POLYGON(('' || to_char(dHL, ''999.9999999999'') || '' '' || to_char((gZone).latitude, ''999.9999999999'') || '','' ||
                               to_char(dHCl, ''999.9999999999'') || '' '' || to_char(dHTop, ''999.9999999999'') || '','' ||
                               to_char(dHCr, ''999.9999999999'') || '' '' || to_char(dHTop, ''999.9999999999'') || '','' ||
                               to_char(dHR, ''999.9999999999'') || '' '' || to_char((gZone).latitude, ''999.9999999999'') || '','' ||
                               to_char(dHCr, ''999.9999999999'') || '' '' || to_char(dHBtm, ''999.9999999999'') || '','' ||
                               to_char(dHCl, ''999.9999999999'') || '' '' || to_char(dHBtm, ''999.9999999999'') || '','' ||
                               to_char(dHL, ''999.9999999999'') || '' '' || to_char((gZone).latitude, ''999.9999999999'') || ''))'';

  RETURN ST_SetSrid(ST_GeometryFromText(tPolygon),4326);
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION geohexGetHexCoordsByZone(_geohexZONE, integer) RETURNS GEOMETRY  AS '
DECLARE
  gZone ALIAS FOR $1;
  iSrid ALIAS FOR $2;
  gGeom geometry;
BEGIN
  gGeom := geohexGetHexCoordsByZone(gZone);
  RETURN ST_Transform(gGeom, iSrid);
END;
' LANGUAGE 'plpgsql';
