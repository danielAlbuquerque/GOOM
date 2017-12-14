-- -------------------------------------------------------------------------------------------------
-- Chuck Woodbury - Senior Systems Consultant, Technology Services.
-- Chuck.Woodbury@HexagonSI.com
-- Hexagon Safety & Infrastructure / Hexagon Geospatial
-- Huntsville, Alabama 35894
-- 256-730-7755
--
-- Copyright (c) 2016, Charles Woodbury
-- All rights reserved.
-- Redistribution and use in source and binary forms, with or without modification, are permitted 
-- provided that the copyright notice, and the following disclaimer are included.
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
-- -------------------------------------------------------------------------------------------------------
-- Note:  GOOM Package must be installed before using this script.
-- ---------------------------------------------------------------------------------------------------------
-- @InsertXY 
-- This script extracts the X and Y values from the columns in a table and writes them to a
-- geometry column that has been added to that table.
-- ----------------------------------------------------------------------------------------
-- History: 01/02/2005  Created.
--          07/06/2005  Modified.
--          04/17/2007  Modified.
--          04/13/2010  Modified to only support Oracle Oriented points.
--          06/17/2014  Updated.
-- ----------------------------------------------------------------------------------------
SET VERIFY OFF
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
PROMPT Extract the X and Y values from the columns in a non spatial table and write ;
PROMPT them to a geometry field that is added to the table automatically.  The result;
PROMPT will be point features.;
EXEC GOOM.DBLLINE;
ACCEPT tab  char  PROMPT 'Enter the table name  ->' DEFAULT 'TEST1';
ACCEPT xcol char  PROMPT 'Enter X coord column  ->' DEFAULT 'X';
ACCEPT ycol char  PROMPT 'Enter Y coord column  ->' DEFAULT 'Y';
EXEC GOOM.DOTLINE;
-- Add a geometry column to the table
ALTER TABLE &&tabname ADD (GEOMETRY  MDSYS.SDO_GEOMETRY);
--
DECLARE 
CURSOR GetROWID is SELECT ROWID FROM &&tabname;
  c_cmd           VARCHAR2(32) := 'InsertXY';
  v_Xcol          VARCHAR2(32) := UPPER('&&xcol');
  v_Ycol          VARCHAR2(32) := UPPER('&&ycol');
  v_table         VARCHAR2(30) := UPPER('&&tab');
  v_commit        PLS_INTEGER  := 500;
  v_count         PLS_INTEGER  := 0;
  v_temp_geom     MDSYS.SDO_GEOMETRY := MDSYS.SDO_GEOMETRY(2001, NULL, NULL, MDSYS.SDO_ELEM_INFO_ARRAY(1,1,1,3,1,0),MDSYS.SDO_ORDINATE_ARRAY());
  v_rowid         ROWID;
  v_xcoord        NUMBER;
  v_ycoord        NUMBER;
  v_sql           VARCHAR2(512);
  v_debug         VARCHAR2(255);
--
BEGIN
    v_debug:='Extract the X and Y values and assign them to the geometry';
    FOR v_rowid IN GetROWID LOOP
      v_sql := 'SELECT '|| v_Xcol ||' FROM '|| v_table||' WHERE ROWID = :vid';
      EXECUTE IMMEDIATE v_sql INTO v_xcoord USING v_rowid.ROWID;
      v_sql := 'SELECT '|| v_Ycol ||' FROM '|| v_table||' WHERE ROWID = :vid';
      EXECUTE IMMEDIATE v_sql INTO v_ycoord USING v_rowid.ROWID;
      v_debug:='Extend the ordiante array to hold the values';
      v_temp_geom.sdo_ordinates.extend (4);
      v_debug:='Assign the X and Y';
      v_temp_geom.sdo_ordinates(1):= v_xcoord;
      v_temp_geom.sdo_ordinates(2):= v_ycoord;
      v_debug:='Assign Default Rotation';
      v_temp_geom.sdo_ordinates(3):=0;
      v_temp_geom.sdo_ordinates(4):=1;
      v_debug:='Update the existing table with the new geometry';
      v_sql := 'UPDATE '||v_table||' A SET A.GEOMETRY=:geom WHERE ROWID = :vid';
      EXECUTE IMMEDIATE v_sql USING v_temp_geom, v_rowid.ROWID;
      v_vount := v_vount + 1;
      -- Commit if count is 500
      IF v_count = v_commit THEN
        COMMIT;
        v_count := 0;
      END IF;
      v_debug:='Reinitialize the Temp Geometry';
      v_temp_geom:=MDSYS.SDO_GEOMETRY(2001, NULL, NULL, MDSYS.SDO_ELEM_INFO_ARRAY(1,1,1,3,1,0),MDSYS.SDO_ORDINATE_ARRAY());
    END LOOP;
    EXCEPTION
       WHEN OTHERS THEN
         GOOM.REPORT_ERROR( c_cmd, v_id, v_debug,SQLCODE,SQLERRM);
END;
/
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
SET VERIFY ON
--