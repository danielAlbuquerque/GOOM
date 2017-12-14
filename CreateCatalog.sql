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
-- -------------------------------------------------------------------------------------------------
-- Note:  GOOM Package must be installed before using this script.
-- ----------------------------------------------------------------------------------------
-- CreateCatalog
-- Returns a schema catalog for the graphic features in the schema
-- Use ListCatalog to view the results at a later data
--
-- Syntax: @CreateCatalog
--         @ListCatalog   -- to see formatted results.
--
-- History:
-- 06/01/2007   -Final Schema Catalog Command
-- 
-- ----------------------------------------------------------------------------------------
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
EXEC GOOM.TitleLine('Schema Catalog Creation');
EXEC GOOM.DotLine;
--
DECLARE
-- Feature Information Cursor
CURSOR GetFeatureData IS 
         SELECT TABLE_NAME,COLUMN_NAME FROM COLS
          WHERE DATA_TYPE='SDO_GEOMETRY' 
            AND TABLE_NAME IN (SELECT OBJECT_NAME FROM USER_OBJECTS WHERE OBJECT_TYPE IN ('TABLE','VIEW'))
            AND TABLE_NAME NOT LIKE 'BIN$%'
            AND TABLE_NAME NOT LIKE 'MDRT%';
--
  v_geom     COLS.COLUMN_NAME%TYPE;
  v_feature  COLS.TABLE_NAME%TYPE;
  v_layer    VARCHAR2(32);
  --
  v_catname  VARCHAR2(30):='SCHEMA_CATALOG';
  v_GeomType VARCHAR2(16);
  v_GMType   NUMBER(32);
  v_GType    VARCHAR2(4);
  v_count    INTEGER:=0;
  v_view     VARCHAR2(5);
  v_sql      VARCHAR2(255);
BEGIN 
    IF goom.ChkTable( v_catname ) THEN
      GOOM.DropTable( v_catname );
    END IF;
    v_sql:='CREATE TABLE '|| v_catname ||'(FEATURECLASS VARCHAR2(30), GEOM_COL VARCHAR2(30), ROW_COUNT INTEGER, OGTYPE VARCHAR2(4), SPATIAL_TYPE VARCHAR2(16), GDO_TYPE VARCHAR2(32), OBJ_TYPE VARCHAR2(5))';
    EXECUTE IMMEDIATE v_sql;
    FOR v_feature in GetFeatureData LOOP
      v_layer   := v_feature.table_name;
      v_geom    := v_feature.column_name;
      v_Gtype   := GOOM.GetGTYPE( v_layer, v_geom);
      v_GeomType:= GOOM.GetGeomType( v_layer, v_geom);
      v_count   := GOOM.GetCount( v_layer);
      IF GOOM.ChkView( v_layer ) THEN
         v_view:='VIEW';
      ELSE
         v_view:='TABLE';
      END IF;
      IF Goom.ChkMetadata(v_layer) THEN
         v_sql:='SELECT DATA_SUBTYPE FROM GDOSYS.GFIELDMAPPING WHERE OWNER=:vUSER AND table_name=:vlayer and column_name =:vgeom and (DATA_TYPE=32 or DATA_TYPE=33)';
         EXECUTE IMMEDIATE v_sql INTO v_GMType USING USER,v_layer,v_geom;
         v_sql:='INSERT INTO '|| v_catname ||' VALUES (:vlayer,:vgeom,:vcount, :v_type, :vGeomType, :vGMType, :vview)';
         EXECUTE IMMEDIATE v_sql USING v_layer, v_geom, v_count, v_Gtype, v_GeomType, v_GMType, v_view;
      ELSE
         v_sql:='INSERT INTO '|| v_catname ||' VALUES (:vlayer,:vgeom,:vcount,:vGtype, :vGeomType,''NONE'', :vview)';
         EXECUTE IMMEDIATE v_sql USING v_layer, v_geom, v_count, v_Gtype, v_GeomType, v_view;
      END IF;   
    END LOOP;
    COMMIT;
    GOOM.SetGDOSYSMetadata(v_catname);
    GOOM.RESPONSE('NEW CATALOG CREATED','Using table '|| v_catname);
EXCEPTION
  WHEN OTHERS THEN
    GOOM.REPORT_ERROR ('CreateCatalog',USER, v_sql,sqlcode,sqlerrm);
END;  
/
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on