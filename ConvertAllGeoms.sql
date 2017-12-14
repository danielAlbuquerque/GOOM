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
-- Special thanks to Ron Frebis for contributing this.
-- ----------------------------------------------------------------------------------------
-- @ConvertAllGeoms
-- Convert all features in the current schema to either 2D or 3D depending on the setting for
-- v_convert.  @d for 2D conversion, 3D for 3D conversion.  Views are handles correctly but
-- materialized views are not.  Be WARNED, a mat view will cause the process to fail.  Text and
-- Raster feature types are not processed.
--
-- TEST THIS BEFORE YOU APPLY IT TO PRODUCTION DATA!!!
--
-- Note:  GOOM Package must be installed before using this script.
-- ----------------------------------------------------------------------------------------
-- History: 06/10/2011  Created.
------------------------------------------------------------------------
set verify off
--
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
DECLARE
CURSOR GetSpatialTables is 
        SELECT A.OBJECT_NAME, B.COLUMN_NAME
          FROM USER_OBJECTS A, USER_SDO_GEOM_METADATA B 
         WHERE A.OBJECT_TYPE='TABLE' 
           AND A.OBJECT_NAME = B.TABLE_NAME;
--
 c_proc      VARCHAR2(15):='ConvertAllGeoms';
 v_convert   VARCHAR2(2) :='2D';
 v_value     GetSpatialTables%ROWTYPE;
 v_table     VARCHAR2(30);
 v_geometry  VARCHAR2(30);
 v_sql       VARCHAR2(255);
 v_count     INTEGER := 0;
 v_etype     INTEGER;
 v_geom      SDO_GEOMETRY;
--
BEGIN
  FOR v_value IN GetSpatialTables LOOP        -- Collect the table and geometry columns
       v_table    := v_value.object_name;      -- Assign the table name
       v_geometry := v_value.column_name;     -- Assign the geometry column name
       GOOM.Response('Begin conversion process', v_table ||'.'|| v_geometry,27);
       -- Capture first geometry
       v_sql := 'SELECT '|| v_geometry ||' FROM '|| v_table || ' WHERE ROWNUM=1';  
       EXECUTE IMMEDIATE v_sql INTO v_geom;
       -- Get its ETYPE
       v_etype    := NVL( v_geom.sdo_elem_info(2), 0 );
       IF v_etype = 0 THEN  -- Check for custom etype = 0 text, raster, or empty.
         GOOM.Response('Unable to process', v_table ||'.'|| v_geometry || ' is text, raster, or empty.');
       ELSE
         GOOM.DROPSIDX( v_table, v_geometry );  -- Drop the Spatial Index
         -- Delete the Oracle Metadata for the table/column combination.
         v_sql := 'DELETE FROM USER_SDO_GEOM_METADATA WHERE TABLE_NAME = :vtable AND COLUMN_NAME = :vgeometry';
         EXECUTE IMMEDIATE v_sql USING v_table, v_geometry;
         -- Separate update section so errors can be handled.
         BEGIN
           v_count := v_count+1;
           IF v_convert = '2D' THEN  -- allow option to go both ways
             v_sql   :='UPDATE '|| v_table ||' SET '|| v_geometry ||' = GOOM.CONVERT2D('|| v_geometry ||')';
           ELSIF v_convert = '3D' THEN
             v_sql   :='UPDATE '|| v_table ||' SET '|| v_geometry ||' = GOOM.CONVERT3D('|| v_geometry ||')';
           ELSE
             NULL;                   -- will use later for better exception handling
           END IF;
           EXECUTE IMMEDIATE v_sql;
           GOOM.Response( v_table ||'.'|| v_geometry,'Converted to '|| v_convert ||'.' );
         EXCEPTION
           WHEN OTHERS THEN
             ROLLBACK;
             GOOM.REPORT_ERROR( c_proc, v_table||'.'|| v_geometry, v_sql, SQLCODE, SQLERRM);
             GOOM.Response('Conversion failed','ROWNUM='|| v_count);
         END;
         COMMIT;
         -- Reset Oracle metadata and indexing.
         GOOM.SETMBR( v_table, v_geometry );
         GOOM.RTree( v_table, v_geometry );     -- use RTree here since you want to pass both table and column being processed
         GOOM.Response('Conversion Complete', v_table ||'.'|| v_geometry, 27);
       END IF;
  END LOOP;
EXCEPTION
  WHEN OTHERS THEN
    GOOM.REPORT_ERROR( c_proc, v_table||'.'|| v_geometry, v_sql, SQLCODE, SQLERRM);
END;
/
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
--
set verify on
-- ----------------------------------------------------------------------------------------
