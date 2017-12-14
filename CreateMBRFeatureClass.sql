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
-- --------------------------------------------------------------------------------------------------
-- Note:  GOOM Package must be installed before using this script.
-- ----------------------------------------------------------------------------------------
-- @CreateMBRFeatureClass
-- Creates a featureclass that contains the MBR of the specified featureclass's geometry.
-- Depends on AGGR_MBR in Oracle Locator, 10G or later gives the best results.
-- 
-- History: 07/15/2005  Modified.
--          04/17/2007  Modified.
--          03/09/2009  Modified.
--          06/18/2014  Modified.
--
SET VERIFY OFF
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
EXEC GOOM.TitleLine('Creates a new feature class containing the MBR of the existing feature class.');
EXEC GOOM.DblLine;
ACCEPT tablename char   PROMPT 'Enter the tablename to store the MBR ->';
ACCEPT featurename char PROMPT 'Enter the feature class name to use -->';
COLUMN "AVAILABLE GEOMETRY COLUMNS" FORMAT A26;
SELECT column_name "AVAILABLE GEOMETRY COLUMNS" 
  FROM COLS WHERE DATA_TYPE='SDO_GEOMETRY' AND TABLE_NAME=UPPER('&&featurename');
ACCEPT geomname char PROMPT  'Enter Geometry Column to use ->' DEFAULT 'GEOMETRY';
EXEC GOOM.DotLine;
--
DECLARE
 c_cmdname  VARCHAR2(32):='CreateMBRFeatureClass';
 v_result   MDSYS.SDO_GEOMETRY;
 v_table    VARCHAR2(32):=UPPER('&&tablename');
 v_feature  VARCHAR2(32):=UPPER('&&featurename');
 v_geometry VARCHAR2(32):=UPPER('&&geomname'); 
 v_csguid   VARCHAR2(64);
 v_sql      VARCHAR2(1024);
 v_debug    VARCHAR2(255);
 v_cschk    INTEGER;
BEGIN
   IF GOOM.chkTable( v_table ) THEN
     EXECUTE IMMEDIATE 'DROP TABLE '|| v_table;
     GOOM.DeleteOrphanMBR;
     v_debug:='Table Checking and creation';
   END IF;
   GOOM.Response( c_cmdname, v_debug);
   EXECUTE IMMEDIATE 'CREATE TABLE '||v_table||' (ID INTEGER PRIMARY KEY, FEATURE_CLASS VARCHAR2(64),MBR MDSYS.SDO_GEOMETRY)';
   EXECUTE IMMEDIATE 'SELECT SDO_AGGR_MBR ('|| v_geometry ||') FROM '|| v_feature ||'' INTO v_result;
   EXECUTE IMMEDIATE 'INSERT INTO '|| v_table ||' VALUES(1,'''|| v_feature ||''',:geom)' USING v_result;
   COMMIT;
   v_debug:='Checking for existing coordsystem';
   EXECUTE IMMEDIATE 'SELECT count(1) FROM GDOSYS.GEOMETRYPROPERTIES WHERE INDEXID IN (SELECT INDEXID FROM GDOSYS.FIELDLOOKUP WHERE FEATURENAME='''||USER||'.'||v_feature||''' AND FIELDNAME='''||v_geometry||''')' INTO v_cschk;
   GOOM.Response( c_cmdname, v_debug);
   IF v_cschk>0 THEN
     v_sql:='SELECT NVL(GCOORDSYSTEMGUID,''DEFAULT'') FROM GDOSYS.GEOMETRYPROPERTIES WHERE INDEXID IN (SELECT INDEXID FROM GDOSYS.FIELDLOOKUP WHERE FEATURENAME='''||USER||'.'||v_feature||''' AND FIELDNAME='''||v_geometry||''')';
     EXECUTE IMMEDIATE v_sql INTO v_csguid;
   ELSE
     v_csguid:='DEFAULT';
   END IF;
   v_debug:='Finish Up';
   GOOM.SetMBRProj ( v_table);
   GOOM.SetGDOSYSMetadata( v_table,NULL, v_csguid);
   GOOM.SpatialIndex( v_table);
EXCEPTION
  WHEN OTHERS THEN
    GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_debug,sqlcode,sqlerrm);
END;
/
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
SET VERIFY ON