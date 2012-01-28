/*
* This library "geohex-plpgsql" is a PL/pgSQL porting of "GeoHex"
* originally licensed by @sa2da (http://geogames.net)
* under Creative Commons BY-SA 2.1 Japan License.
*
* geohex-plpgsql is licensed by ASAHI Kosuke
* under Creative Commons BY-SA 2.1 Japan License.
*/

DROP FUNCTION IF EXISTS geohexGetIntersectedGeohex(geometry, integer) CASCADE;
DROP FUNCTION IF EXISTS geohexGetNeighborsByLocation(double precision, double precision, integer, integer, integer) CASCADE;
DROP FUNCTION IF EXISTS geohexGetNeighborsByLocation(double precision, double precision, integer, integer) CASCADE;
DROP FUNCTION IF EXISTS geohexGetNeighborsByCode(varchar, integer, integer) CASCADE;
DROP FUNCTION IF EXISTS geohexGetNeighborsByCode(varchar, integer) CASCADE;
DROP FUNCTION IF EXISTS _geohexCalDistance(integer, integer, integer, integer) CASCADE;

DROP TYPE IF EXISTS _geohexHex CASCADE;
CREATE TYPE _geohexHex AS (
  code varchar(17),
  the_geom GEOMETRY
);

CREATE FUNCTION _geohexCalDistance(integer, integer, integer, integer) RETURNS integer AS '
DECLARE
 iX1 ALIAS FOR $1;
 iY1 ALIAS FOR $2;
 iX2 ALIAS FOR $3;
 iY2 ALIAS FOR $4;
 idX integer;
 idY integer;
BEGIN
 idX = iX2 - iX1;
 idY = iY2 - iY1;
 IF (idX * idY) >= 0 THEN
   IF abs(idX) > abs(idY) THEN
     RETURN abs(idX);
   ELSE
     RETURN abs(idY);
   END IF;
 ELSE
   RETURN abs(idX - idY);
 END IF;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION geohexGetNeighborsByLocation(double precision, double precision, integer, integer) RETURNS SETOF _geohexHex AS '
DECLARE
  dLat ALIAS FOR $1;
  dLon ALIAS FOR $2;
  iLevel ALIAS FOR $3;
  iDistance ALIAS FOR $4;
  dHSize double precision;
  gZone _geohexZONE;
  gLoc _geohexLOC;
  iX integer;
  iY integer;
  gNeighbor _geohexZONE;
  gHex RECORD;
  dHLatX double precision;
  dHLatY double precision;
  dUnitX double precision;
  dUnitY double precision;
  dHX double precision;
  dHY double precision;
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

  gZone := geohexGetZoneByLocation(dLat, dLon, iLevel);
  dHSize := _geohexCalcHexSize((gZone).level+2);
  dUnitX := 6 * dHSize;
  dUnitY := 6 * dHSize * _geohexHK();
    
  FOR iX IN -iDistance..iDistance LOOP
    FOR iY IN -iDistance..iDistance LOOP
      IF _geohexCalDistance(0,0,iX,iY) > iDistance THEN
        CONTINUE;
      END IF;
      dHLatY := (_geohexHK() * ((gZone).x + iX) * dUnitX + ((gZone).y + iY) * dUnitY) / 2;
      dHLatX := (dHLatY - ((gZone).y + iY) * dUnitY) / _geohexHK();
      gLoc := _geohexXy2loc(dHLatX, dHLatY);
      gNeighbor := geohexGetZoneByLocation((gLoc).lat, (gLoc).lon, (gZone).level);
      gHex := ROW((gNeighbor).code, geohexGetHexCoordsByZone(gNeighbor));
      RETURN NEXT gHex;
    END LOOP;
  END LOOP;
  RETURN;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION geohexGetNeighborsByLocation(double precision, double precision, integer, integer, integer) RETURNS SETOF _geohexHex AS '
DECLARE
  dLat ALIAS FOR $1;
  dLon ALIAS FOR $2;
  iLevel ALIAS FOR $3;
  iDistance ALIAS FOR $4;
  iSrid  ALIAS FOR $5;
  gCHex _geohexHex;
  gHex RECORD;
  cur refcursor;
BEGIN
  OPEN cur FOR select * from geohexGetNeighborsByLocation(dLat, dLon, iLevel, iDistance);
  LOOP
    FETCH cur INTO gCHex;
    IF NOT FOUND THEN
      EXIT;
    END IF;
    gHex := ROW((gCHex).code, ST_Transform((gCHex).the_geom, iSrid));
    RETURN NEXT gHex;
  END LOOP;
  CLOSE cur;
  RETURN;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION geohexGetNeighborsByCode(varchar, integer) RETURNS SETOF _geohexHex AS '
DECLARE
  cCode ALIAS FOR $1;
  iDistance ALIAS FOR $2;
  dHSize double precision;
  gZone _geohexZONE;
  gLoc _geohexLOC;
  iX integer;
  iY integer;
  gNeighbor _geohexZONE;
  gPoly GEOMETRY;
  gHex _geohexHex;
  dHLatX double precision;
  dHLatY double precision;
  dUnitX double precision;
  dUnitY double precision;
  dHX double precision;
  dHY double precision;
BEGIN
  IF cCode !~  ''^[A-Za-z][A-Za-z][0-9]*$'' THEN
    RAISE EXCEPTION ''%s is notGeoHex Code'', cCode;
  END IF;
  
  gZone := geohexGetZoneByCode(cCode);
  dHSize := _geohexCalcHexSize((gZone).level+2);
  dUnitX := 6 * dHSize;
  dUnitY := 6 * dHSize * _geohexHK();
    
  FOR iX IN -iDistance..iDistance LOOP
    FOR iY IN -iDistance..iDistance LOOP
      IF _geohexCalDistance(0,0,iX,iY) > iDistance THEN
        CONTINUE;
      END IF;
      dHLatY := (_geohexHK() * ((gZone).x + iX) * dUnitX + ((gZone).y + iY) * dUnitY) / 2;
      dHLatX := (dHLatY - ((gZone).y + iY) * dUnitY) / _geohexHK();
      gLoc := _geohexXy2loc(dHLatX, dHLatY);
      gNeighbor := geohexGetZoneByLocation((gLoc).lat, (gLoc).lon, (gZone).level);
      gHex := ROW((gNeighbor).code, geohexGetHexCoordsByZone(gNeighbor));
      RETURN NEXT gHex;
    END LOOP;
  END LOOP;
  RETURN;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION geohexGetNeighborsByCode(varchar, integer, integer) RETURNS SETOF _geohexHex AS '
DECLARE
  cCode ALIAS FOR $1;
  iDistance ALIAS FOR $2;
  iSrid  ALIAS FOR $3;
  gCHex _geohexHex;
  gHex RECORD;
  cur refcursor;
BEGIN
  OPEN cur FOR select * from geohexGetNeighborsByCode(cCode, iDistance);
  LOOP
    FETCH cur INTO gCHex;
    IF NOT FOUND THEN
      EXIT;
    END IF;
    gHex := ROW((gCHex).code, ST_Transform((gCHex).the_geom, iSrid));
    RETURN NEXT gHex;
  END LOOP;
  CLOSE cur;
  RETURN;
END;
' LANGUAGE 'plpgsql';

CREATE FUNCTION geohexGetIntersectedGeohex(geometry, integer) RETURNS SETOF _geohexHex AS '
DECLARE
  gGeom ALIAS FOR $1;
  iLevel ALIAS FOR $2;
  gBound geometry;
  iSrid integer;
  dLat double precision;
  dLon double precision;
  gCZone _geohexZONE;
  gRUZone _geohexZONE;
  iDistance integer;
  i integer;
  gCHex _geohexHex;
  gHex RECORD;
  cur refcursor;
BEGIN
  iSrid = ST_SRID(gGeom);
  gBound = ST_Transform(ST_Boundary(gGeom), 4326);
  dLat = (ST_YMax(gBound)+ST_YMin(gBound))/2;
  dLon = (ST_XMax(gBound)+ST_XMin(gBound))/2;
  gCZone = geohexGetZoneByLocation(dLat, dLon, iLevel);
  gRUZone = geohexGetZoneByLocation(ST_YMax(gBound), ST_XMax(gBound), iLevel);
  iDistance = _geohexCalDistance((gCZone).x, (gCZone).y, (gRUZone).x, (gRUZone).y) + 1;

  OPEN cur FOR select * from geohexGetNeighborsByLocation(dLat, dLon, iLevel, iDistance, iSrid);
  LOOP
    FETCH cur INTO gCHex;
    IF NOT FOUND THEN
      EXIT;
    END IF;
    IF ST_Intersects(gGeom, (gCHex).the_geom) THEN
      RETURN NEXT gCHex;
    END IF;
  END LOOP;
  CLOSE cur;
  RETURN;
END;
' LANGUAGE 'plpgsql';