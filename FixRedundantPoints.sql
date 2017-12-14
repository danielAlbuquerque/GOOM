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
-- -------------------------------------------------------------------------------------------------
-- History:  07/06/2005  Modified as an example.
--           04/17/2007  Modified.
--           04/26/2010  Modified to use owner.table
--           06/18/2014   Modified.
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET TERMOUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
--
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('Fix redundant points in the specified feature class');
EXEC GOOM.DblLine;
Accept tablename CHAR PROMPT 'Enter Feature Class Name -----> ';
SET TERMOUT OFF
VARIABLE vownertable VARCHAR2(61);
VARIABLE vowner      VARCHAR2(30);
VARIABLE vtable      VARCHAR2(30);
VARIABLE vgeom       VARCHAR2(30);
VARIABLE vtol        NUMBER;
EXEC :vownertable := GOOM.GetOwnerObject(UPPER('&tablename'));
EXEC :vowner      := GOOM.SplitOwnerObject(:vownertable,'OWNER');
EXEC :vtable      := GOOM.SplitOwnerObject(:vownertable,'TABLE');
SET TERMOUT ON
SELECT column_name "AVAILABLE GEOMETRY COLUMNS" 
  FROM ALL_TAB_COLUMNS 
 WHERE DATA_TYPE='SDO_GEOMETRY' AND TABLE_NAME=:vtable AND OWNER=:vowner;
PROMPT ;
Accept geometry CHAR  PROMPT 'Enter (GEOMETRY) Col Name --> ';
SET TERMOUT OFF
EXEC :vgeom := NVL(UPPER('&geometry'),'GEOMETRY');
SET TERMOUT ON
ACCEPT tolerance NUMBER PROMPT 'Enter the tolerance to use ->' DEFAULT 0.0001;
SET TERMOUT OFF
EXEC :vtol := NVL(&tolerance,0.0001);
SET TERMOUT ON
EXEC GOOM.DotLine;
EXEC GOOM.FixRedundantPoints(:vownertable, :vgeom, :vtol);
EXEC GOOM.DblLine;
-- ----------------------------------------------------------------------------------------
SET FEEDBACK ON
SET TIMING ON
-- ----------------------------------------------------------------------------------------