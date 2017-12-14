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
-- -------------------------------------------------------------------------------------------------------
-- Note:  GOOM Package must be installed before using this script.
-- -------------------------------------------------------------------------------------------------------
-- @FixSmallArcs
-- Fixes tiny arcs so GeoMedia can handle them correctly.  It does this by stroking the
-- arcs into line strings.
-- ----------------------------------------------------------------------------------------
-- History: 05/30/2003 Created
--          04/10/2005 Updated
--          07/02/2005 Updated
--          04/26/2010 Support for owner.table and new GOOM routines.
--          06/18/2014 Modified.
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET TERMOUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('Stroke very small arcs in the specified feature class');
EXEC GOOM.DblLine;
Accept tablename CHAR PROMPT 'Enter Feature Class Name ---> ';
--
-- Gather geometry column information so user can choose which geometry to use.
SET TERMOUT OFF
VARIABLE vownertable VARCHAR2(61);
VARIABLE vowner      VARCHAR2(30);
VARIABLE vtable      VARCHAR2(30);
VARIABLE vgeom       VARCHAR2(30);
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
EXEC GOOM.DotLine;
exec GOOM.FixSmallArcs(:vownertable, :vgeom);
EXEC GOOM.DblLine;
-- ----------------------------------------------------------------------------------------
SET FEEDBACK ON
SET TIMING ON
-- ----------------------------------------------------------------------------------------