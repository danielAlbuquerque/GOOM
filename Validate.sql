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
-- @validate
-- This script validates the geometries stored in the specified feature 
-- class or layer. The script calls the Validategeom procedure in the
-- GOOM_PKG.
-- ----------------------------------------------------------------------------------------
-- History:  07/02.2005  Updated.
--           10/12/2005  Added option to create bad data view.
--           04/17/2007  Modified.
--           06/11/2012  Added error handling for no primary key.
--           06/17/2014  Added error handling for no primary key.
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
--
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('Validate:  This script will validate the contents of a geometry field.');
EXEC GOOM.TitleLine('Any results will be stored in <table>_ERR and you can optionally view the');
EXEC GOOM.TitleLine('bad data in GeoMedia via a view called <table>_BAD.');
EXEC GOOM.DblLine;
ACCEPT ctable char  PROMPT 'Enter Feature Class to use  --->' DEFAULT 'CITIES';
COLUMN "AVAILABLE GEOMETRY COLUMNS" FORMAT A26;
SELECT column_name "AVAILABLE GEOMETRY COLUMNS" FROM COLS WHERE DATA_TYPE='SDO_GEOMETRY' AND TABLE_NAME=UPPER('&ctable');
PROMPT ;
ACCEPT geom char   PROMPT 'Enter Geometry Column to use  -->' DEFAULT 'GEOMETRY';
ACCEPT mkview char PROMPT 'Create view of bad data (Y)/N -->' DEFAULT 'Y';
EXEC GOOM.DotLine;
DECLARE
 v_tablename  VARCHAR2(32):=UPPER('&ctable');
 v_geometry   VARCHAR2(32):=UPPER('&geom');
 v_mkview     VARCHAR2(32):=UPPER('&mkview');
 v_resulttab  VARCHAR2(32):=RTRIM(SUBSTR( v_tablename,1,26))||'_ERR';
 v_badtab     VARCHAR2(32):=RTRIM(SUBSTR( v_tablename,1,26))||'_BAD';
 v_sql        VARCHAR2(1024);
 v_pkey       VARCHAR2(32);
BEGIN
  GOOM.ValidateGeom( v_tablename, v_geometry);
  IF v_mkview = 'Y' AND GOOM.chkTable( v_resulttab ) THEN
    v_sql:='SELECT COLUMN_NAME FROM USER_IND_COLUMNS WHERE INDEX_NAME in
          (select CONSTRAINT_NAME from USER_CONSTRAINTS where CONSTRAINT_TYPE=''P'' and TABLE_NAME='''|| v_tablename ||''')';
    EXECUTE IMMEDIATE v_sql into v_pkey;
    v_sql:='CREATE OR REPLACE VIEW '|| v_badtab||' AS SELECT A.'|| v_pkey ||',A.'|| v_geometry ||', B.RESULT, B.DESCRIPTION FROM '|| v_tablename ||' A,'|| v_resulttab ||' B  where A.ROWID = B.SDO_ROWID';
    EXECUTE IMMEDIATE v_sql;
    GOOM.RESPONSE('VALIDATION','View '|| v_badtab||' has been created.');
    GOOM.SetMBRProj( v_badtab );
    GOOM.SetGDOSYSMetadata(v_badtab,NULL,'DEFAULT',NULL,v_pkey);
  END IF;
EXCEPTION
  WHEN NO_DATA_FOUND THEN
    GOOM.REPORT_ERROR ('VALIDATE', v_sql, 'No primary key found in table: '|| v_tablename, 'A Primary key is required to create view: '|| v_badtab, sqlerrm);
  WHEN OTHERS THEN
    IF GOOM.chkTable(v_resulttab) THEN
      EXECUTE IMMEDIATE 'DROP TABLE '|| v_resulttab;
    END IF;
    IF GOOM.chkView(v_badtab) THEN
      EXECUTE IMMEDIATE 'DROP VIEW '|| v_badtab;
    END IF;
    GOOM.REPORT_ERROR ('VALIDATE', v_sql, v_resulttab, sqlcode, sqlerrm);
END;
/
--
EXEC GOOM.DblLine;
SET FEEDBACK ON
SET TIMING ON
-- ----------------------------------------------------------------------------------------