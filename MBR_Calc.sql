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
-- -------------------------------------------------------------------------------------------------------
-- @MBR_CALC  -- follow prompts.
-- Returns the extents of a layer for use in USER_SDO_GEOM_METADATA.
-- ----------------------------------------------------------------------------------------
-- History: 01/01/2003  Created.
--          04/10/2005  Modified.
--          07/02/2005  Modified.
--          04/17/2007  Modified.
--          04/26/2010  Modified to use owner.table
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET TERMOUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('Calculate extents for the specified feature class');
EXEC GOOM.DblLine;
Accept tablename CHAR PROMPT 'Enter Feature Class Name ---> ';
-- Gather geometry column information so user can choose which geometry to use.
SET TERMOUT OFF
VARIABLE vownertable VARCHAR2(61);
VARIABLE vowner      VARCHAR2(30);
VARIABLE vtable      VARCHAR2(30);
VARIABLE vgeom       VARCHAR2(30);
VARIABLE vsrid       NUMBER;
EXEC :vownertable := GOOM.GetOwnerObject(UPPER('&tablename'));
EXEC :vowner      := GOOM.SplitOwnerObject(:vownertable,'OWNER');
EXEC :vtable      := GOOM.SplitOwnerObject(:vownertable,'TABLE');
SET TERMOUT ON
SELECT column_name "AVAILABLE GEOMETRY COLUMNS" 
  FROM ALL_TAB_COLUMNS 
 WHERE DATA_TYPE='SDO_GEOMETRY' AND TABLE_NAME=:vtable AND OWNER=:vowner;
PROMPT ;
Accept geometry CHAR   PROMPT 'Enter (GEOMETRY) Col Name --> ';
EXEC :vgeom := NVL(UPPER('&geometry'),'GEOMETRY');
-- Geometry column information gathered.
Declare
c_cmdname VARCHAR2(12):='MBR_CALC';
v_results mdsys.sdo_geometry;
v_xmin NUMBER(24,6);
v_ymin NUMBER(24,6);
v_zmin NUMBER(24,6);
v_xmax NUMBER(24,6);
v_ymax NUMBER(24,6);
v_zmax NUMBER(24,6);
v_dim  INTEGER;
v_xmin_est NUMBER;
v_ymin_est NUMBER;
v_zmin_est NUMBER;
v_xmax_est NUMBER;
v_ymax_est NUMBER;
v_zmax_est NUMBER;
x_adjust   NUMBER;
y_adjust   NUMBER;
z_adjust   NUMBER;
v_buffer   NUMBER:=0.05;
v_debug    VARCHAR2(255):='START';
v_sql      VARCHAR2(255);
--
begin
   v_dim := TO_NUMBER(GOOM.GetDim(:vownertable,:vgeom),9); 
   v_sql := 'SELECT SDO_AGGR_MBR('||:vgeom||') FROM '|| :vownertable;
   EXECUTE IMMEDIATE v_sql INTO v_results;
   v_debug :='SDO_AGGR_MBR';
   IF v_dim<3 THEN
     v_xmin:=v_results.sdo_ordinates(1);
     v_ymin:=v_results.sdo_ordinates(2);
     v_xmax:=v_results.sdo_ordinates(3);
     v_ymax:=v_results.sdo_ordinates(4);
     -- Add a small buffer and round the values
     x_adjust:=ABS(v_xmax-v_xmin) * v_buffer;
     y_adjust:=ABS(v_ymax-v_ymin) * v_buffer;
     v_xmin_est:=ROUND(v_xmin-x_adjust,0);
     v_ymin_est:=ROUND(v_ymin-y_adjust,0);
     v_xmax_est:=ROUND(v_xmax+x_adjust,0);
     v_ymax_est:=ROUND(v_ymax+y_adjust,0);
     v_debug:='Completed 2D estimates.';
     --
     GOOM.DBMSG('========================== 2-D =================================');
     GOOM.DBMSG('The minimum bounding rectangle (MBR) for '||:vownertable||':');
     GOOM.DBMSG('------------------------------------------------');
     GOOM.DBMSG('X MIN= '||v_xmin||'  Adjusted (10%): '||v_xmin_est     );
     GOOM.DBMSG('Y MIN= '||v_ymin||'  Adjusted (10%): '||v_ymin_est     );
     GOOM.DBMSG('------------------------------------------------');
     GOOM.DBMSG('X MAX= '||v_xmax||'  Adjusted (10%): '||v_xmax_est     );
     GOOM.DBMSG('Y MAX= '||v_ymax||'  Adjusted (10%): '||v_ymax_est     );
     GOOM.DBMSG('-------------------------------------------------------');
     GOOM.DBMSG(' Copy/Paste the following to update the metadata table' );
     GOOM.DBMSG('-------------------------------------------------------');
     GOOM.DBMSG('DELETE FROM USER_SDO_GEOM_METADATA WHERE TABLE_NAME='''||:vownertable||''';');
     GOOM.DBMSG('INSERT INTO USER_SDO_GEOM_METADATA VALUES');
     GOOM.DBMSG('('''||:vownertable||''','''||:vgeom||''', MDSYS.SDO_DIM_ARRAY(');
     GOOM.DBMSG('MDSYS.SDO_DIM_ELEMENT(''X'','||v_xmin_est||','|| v_xmax_est||', 0.00005),');
     GOOM.DBMSG('MDSYS.SDO_DIM_ELEMENT(''Y'','||v_ymin_est||','|| v_ymax_est||', 0.00005)),NULL);');
     GOOM.DBMSG('COMMIT;');
   ELSE
     v_xmin:=v_results.sdo_ordinates(1);
     v_ymin:=v_results.sdo_ordinates(2);
     v_zmin:=v_results.sdo_ordinates(3);
     v_xmax:=v_results.sdo_ordinates(4);
     v_ymax:=v_results.sdo_ordinates(5);
     v_zmax:=v_results.sdo_ordinates(6);
     -- Add a small buffer and round the values
     x_adjust:=ABS(v_xmax-v_xmin) * v_buffer;
     y_adjust:=ABS(v_ymax-v_ymin) * v_buffer;
     z_adjust:=ABS(v_zmax-v_zmin) * v_buffer;
     v_xmin_est:=ROUND(v_xmin-x_adjust,0);
     v_ymin_est:=ROUND(v_ymin-y_adjust,0);
     v_zmin_est:=ROUND(v_zmin-z_adjust,0);
     v_xmax_est:=ROUND(v_xmax+x_adjust,0);
     v_ymax_est:=ROUND(v_ymax+x_adjust,0);
     v_zmax_est:=ROUND(v_zmax+z_adjust,0);
     v_debug:='Completed 3D estimates.';
     --
     GOOM.DBMSG('============================== 3-D =============================');
     GOOM.DBMSG('The minimum bounding rectangle (MBR) for '||:vownertable||':');
     GOOM.DBMSG('------------------------------------------------');
     GOOM.DBMSG('X MIN= '||v_xmin||'  Adjusted (10%): '||v_xmin_est     );
     GOOM.DBMSG('Y MIN= '||v_ymin||'  Adjusted (10%): '||v_ymin_est     );
     GOOM.DBMSG('Z MIN= '||v_zmin||'  Adjusted (10%): '||v_zmin_est     );
     GOOM.DBMSG('------------------------------------------------');
     GOOM.DBMSG('X MAX= '||v_xmax||'  Adjusted (10%): '||v_xmax_est     );
     GOOM.DBMSG('Y MAX= '||v_ymax||'  Adjusted (10%): '||v_ymax_est     );
     GOOM.DBMSG('Z MAX= '||v_zmax||'  Adjusted (10%): '||v_zmax_est     );
     GOOM.DBMSG('-------------------------------------------------------');
     GOOM.DBMSG(' Copy/Paste the following to update the metadata table.' );
     IF :vowner <> USER THEN
     GOOM.DBMSG(' WARNING: Only the owner of the table can do this. You');
     GOOM.DBMSG(' are not the owner of the table '||:vtable||'.' );       
     END IF;
     GOOM.DBMSG('-------------------------------------------------------');
     GOOM.DBMSG('DELETE FROM USER_SDO_GEOM_METADATA WHERE TABLE_NAME='''||:vtable||''';');
     GOOM.DBMSG('INSERT INTO USER_SDO_GEOM_METADATA VALUES');
     GOOM.DBMSG('('''||:vtable||''','''||:vgeom||''', MDSYS.SDO_DIM_ARRAY(');
     GOOM.DBMSG('MDSYS.SDO_DIM_ELEMENT(''X'','|| v_xmin_est||','|| v_xmax_est||', 0.00005),');
     GOOM.DBMSG('MDSYS.SDO_DIM_ELEMENT(''Y'','|| v_ymin_est||','|| v_ymax_est||', 0.00005),');
     GOOM.DBMSG('MDSYS.SDO_DIM_ELEMENT(''Z'','|| v_zmin_est||','|| v_zmax_est||', 0.00005)),NULL);');
     GOOM.DBMSG('COMMIT;');
   END IF;
EXCEPTION
        WHEN OTHERS THEN
          GOOM.REPORT_ERROR (c_cmdname,:vownertable||'.'||:vgeom,v_debug,sqlcode,sqlerrm);
END;
/
EXEC GOOM.DblLine;
-- ----------------------------------------------------------------------------------------
SET FEEDBACK ON
SET TIMING ON
-- ----------------------------------------------------------------------------------------