-- -------------------------------------------------------------------------------------------------
-- Chuck Woodbury - Senior Systems Consultant, Technology Services.
-- Chuck.Woodbury@HexagonSI.com
-- Hexagon Safety & Infrastructure / Hexagon Geospatial
-- Huntsville, Alabama 35894
-- 256-730-7755
--
-- Copyright (c) 2016, Charles Woodbury
-- All rights reserved.
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
-- copyright notice, and the following disclaimer are included.
-- -------------------------------------------------------------------------------------------------
-- DISCLAIMER
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
-- FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
-- DAMAGES (NOT LIMITED TO THE LOSS OF USE, DATA, OR PROFITS, OR BUSINESS INTERRUPTION HOWEVER
-- CAUSED INCLUDING, AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
-- OR TORT, INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
-- EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- ---------------------------------------------------------------------------------------------------
-- @GMpt2OOpt
-- Convert an Oracle Native Point or a GeoMedia Oriented Point to an Oracle Oriented Point.
-- This is for Oracle 10G and GM applications 6.0 or later.
-- 
-- Note:  GOOM Package must be installed before using this script.
-- ----------------------------------------------------------------------------------------
-- History:  08/02/2005   Created.
--           04/17/2007   Modified.
--           02/25/2010   Bug Fixes - from Joerg Rittmann
--           04/26/2010   Support for owner.table and new GOOM routines.
--           04/15/2015   Desupported.   This script is no longer necessary.
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
-- Input Section
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('Convert GeoMedia 6.0 Oriented Points to Oracle Oriented Points');
EXEC GOOM.DblLine;
Accept table_name CHAR PROMPT 'Enter Feature Class Name ---> ';
--
-- Gather geometry column information so user can choose which geometry to use.
SET TERMOUT OFF
VARIABLE vownertable VARCHAR2(61);
VARIABLE vowner      VARCHAR2(30);
VARIABLE vtable      VARCHAR2(30);
VARIABLE vgeom       VARCHAR2(30);
EXEC :vownertable := GOOM.GetOwnerObject(UPPER('&table_name'));
EXEC :vowner      := GOOM.SplitOwnerObject(:vownertable,'OWNER');
EXEC :vtable      := GOOM.SplitOwnerObject(:vownertable,'TABLE');
SET TERMOUT ON
SELECT column_name "AVAILABLE GEOMETRY COLUMNS" 
  FROM ALL_TAB_COLUMNS 
 WHERE DATA_TYPE='SDO_GEOMETRY' AND TABLE_NAME=:vtable AND OWNER=:vowner;
PROMPT ;
Accept geometry CHAR   PROMPT 'Enter (GEOMETRY) Col Name --> ' DEFAULT 'GEOMETRY';
EXEC :vgeom := UPPER('&geometry');
-- Geometry column information gathered.
EXEC GOOM.DotLine;
DECLARE
  c_cmdname    VARCHAR2(32):='GMpt2OOpt';
  v_tablename  VARCHAR2(61):= :vownertable;
  v_geometry   VARCHAR2(32):= :vgeom;
  --
  v_sql        VARCHAR2(1024);
  v_debug      VARCHAR2(1024);
  v_GTYPE      VARCHAR2(4);
  v_dim        INTEGER;
  v_rowid      ROWID;
  v_pttype     VARCHAR2(32);
  v_geom       MDSYS.SDO_GEOMETRY;
  v_2dgeom     MDSYS.SDO_GEOMETRY;
  v_3dgeom     MDSYS.SDO_GEOMETRY;
  v_cur        INTEGER;
  v_dum        INTEGER;
  v_count      INTEGER:=0;
  v_procflag   BOOLEAN:=TRUE;
  v_xcoord     FLOAT:=0;
  v_ycoord     FLOAT:=0;
  v_zcoord     FLOAT:=0;
  v_irot       FLOAT:=0;
  v_jrot       FLOAT:=0;
  v_krot       FLOAT:=0;
BEGIN
-- Check for compound geometry
v_sql:='SELECT COUNT(UNIQUE(A.'|| v_geometry ||'.SDO_GTYPE)) FROM '|| v_tablename ||' A';
EXECUTE IMMEDIATE v_sql into v_count;
IF v_count=1 THEN 
  v_sql:='SELECT A.'|| v_geometry ||' FROM '|| v_tablename ||' A where ROWNUM=1';
  EXECUTE IMMEDIATE v_sql INTO v_geom;
  v_GTYPE:=v_geom.SDO_GTYPE; 
  v_dim:=SUBSTR(v_GTYPE,1,1);
  IF v_GTYPE = '2001' OR v_GTYPE = '3001' THEN
    IF v_GEOM.SDO_POINT.X IS NULL THEN
      IF v_GEOM.SDO_ELEM_INFO(3)=6000 AND v_GEOM.SDO_ELEM_INFO(2)=0 THEN
        v_pttype:='GEOMEDIA';
        v_procflag:=TRUE;
      ELSE
        v_pttype:='ORACLE';
        GOOM.DBMSG('Point is already converted or type is unknown.');
        v_procflag:=FALSE;
      END IF;
    ELSE
      v_pttype:='NATIVE';
      v_procflag:=TRUE;
    END IF;
  ELSE
    GOOM.DBMSG('This is not a point geometry.');
    v_procflag:=FALSE;
  END IF;
ELSE
  GOOM.DBMSG('COMPOUND Geometries are not supported.');
  v_procflag:=FALSE;
END IF;
--
IF v_procflag THEN
v_sql:='SELECT ROWID FROM '|| v_tablename;         -- Use the rowids to update the table
v_cur:=GOOM.PARSESQL(v_sql);                      -- One cursor and parse statement
DBMS_SQL.DEFINE_COLUMN_ROWID( v_cur, 1, v_rowid);
v_dum:= DBMS_SQL.EXECUTE(v_cur);
LOOP                                              -- Loop thru table
  IF( DBMS_SQL.FETCH_ROWS(v_cur) = 0 ) THEN        -- exit when all rows retreived
    EXIT;
  END IF;
  DBMS_SQL.COLUMN_VALUE_ROWID( v_cur, 1, v_rowid );
  v_sql:='SELECT A.'|| v_geometry ||' FROM '|| v_tablename ||' A WHERE ROWID=:vrowid';
  EXECUTE IMMEDIATE v_sql INTO v_geom USING v_rowid;
  CASE v_pttype
    WHEN 'GEOMEDIA' THEN
      CASE v_dim
       WHEN 2 THEN
        v_irot:=v_geom.sdo_ordinates(1);
        v_jrot:=v_geom.sdo_ordinates(2);
        v_xcoord:=v_geom.sdo_ordinates(4);
        v_ycoord:=v_geom.sdo_ordinates(5);
       WHEN 3 THEN
        v_irot:=v_geom.sdo_ordinates(1);
        v_jrot:=v_geom.sdo_ordinates(2);
        v_krot:=v_geom.sdo_ordinates(3);
        v_xcoord:=v_geom.sdo_ordinates(4);
        v_ycoord:=v_geom.sdo_ordinates(5);
        v_zcoord:=v_geom.sdo_ordinates(6);
       ELSE
        GOOM.DBMSG('Warning:  Incorrect Point Dimensions.');
      END CASE;
    WHEN 'NATIVE' THEN
      CASE v_dim
       WHEN 2 THEN
        v_xcoord:=v_geom.sdo_point.X;
        v_ycoord:=v_geom.sdo_point.Y;
        v_irot:=1;
        v_jrot:=0;
       WHEN 3 THEN
        v_xcoord:=v_geom.sdo_point.X;
        v_ycoord:=v_geom.sdo_point.Y;
        v_zcoord:=v_geom.sdo_point.Z;
        v_irot:=1;
        v_jrot:=0;
        v_krot:=0;
       ELSE
        GOOM.DBMSG('Warning:  Incorrect Point Dimensions.');
      END CASE;
    END CASE;
    CASE v_dim
       WHEN 2 THEN
         v_2dgeom:=MDSYS.SDO_GEOMETRY(2001, NULL, NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1,1,3,1,0),MDSYS.SDO_ORDINATE_ARRAY());
         v_2dgeom.sdo_ordinates.extend(4);
         v_2dgeom.sdo_ordinates(1):= v_xcoord;
         v_2dgeom.sdo_ordinates(2):= v_ycoord;
         v_2dgeom.sdo_ordinates(3):= v_irot;
         v_2dgeom.sdo_ordinates(4):= v_jrot;
         v_sql:='UPDATE '|| v_tablename ||' A SET A.'|| v_geometry ||'=:geom where ROWID=:vrowid'; 
         EXECUTE IMMEDIATE v_sql USING v_2dgeom, v_rowid;
         COMMIT;
       WHEN 3 THEN
         v_3dgeom:=MDSYS.SDO_GEOMETRY(3001, NULL, NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1,1,4,1,0),MDSYS.SDO_ORDINATE_ARRAY());
         v_3dgeom.sdo_ordinates.extend (6);
         v_3dgeom.sdo_ordinates(1):=v_xcoord;
         v_3dgeom.sdo_ordinates(2):=v_ycoord;
         v_3dgeom.sdo_ordinates(3):=v_zcoord;
         v_3dgeom.sdo_ordinates(4):=v_irot;
         v_3dgeom.sdo_ordinates(5):=v_jrot;
         v_3dgeom.sdo_ordinates(6):=v_krot;
         v_sql:='UPDATE '||v_tablename||' A SET A.'||v_geometry||'=:geom where ROWID=:vrowid'; 
         EXECUTE IMMEDIATE v_sql USING v_3dgeom, v_rowid;
         COMMIT;
       ELSE
        GOOM.DBMSG('Warning:  Incorrect Point Dimensions.');
      END CASE;
END LOOP;
END IF;
EXCEPTION
  WHEN OTHERS THEN
     GOOM.REPORT_ERROR (c_cmdname, v_sql, v_debug, sqlcode, sqlerrm);
END;
/
show errors
--
EXEC GOOM.DblLine;
SET FEEDBACK ON
SET TIMING ON
