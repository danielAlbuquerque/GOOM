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
-- @ChngPrimaryGeom
-- Change the primary geometry column within a feature class. Only 
-- one geometry field can be primary.  Then primary geometry is the 
-- only geometry that can be editied.
-- History: 06/2004  Created
--          07/2005  Updated
--          04/2010  Support for owner.table and new GOOM routines.
--
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
-- Input Section
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('Change the primary geometry column of a feature class');
EXEC GOOM.DblLine;
Accept table_name CHAR PROMPT 'Enter Feature Class Name ---> ';
EXEC GOOM.DotLine;
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
-- This will show you which is primary
COLUMN "CURRENT PRIMARY GEOMETRY"   FORMAT A30;
COLUMN "NEW PRIMARY GEOMETRY"       FORMAT A30;
SELECT PRIMARYGEOMETRYFIELDNAME "CURRENT PRIMARY GEOMETRY" FROM GDOSYS.GFEATURES where FEATURENAME= :vownertable;
PROMPT ;
Accept geometry CHAR   PROMPT 'Enter new primary (GEOMETRY) Col Name --> ';
EXEC :vgeom := NVL(UPPER('&geometry'),'GEOMETRY');
-- This allows you to change the primary geometry on one of the secondaries.
UPDATE GDOSYS.GFEATURES SET PRIMARYGEOMETRYFIELDNAME= :vgeom where FEATURENAME= :vownertable;
--
COMMIT;
EXEC GOOM.DotLine;
SELECT PRIMARYGEOMETRYFIELDNAME "NEW PRIMARY GEOMETRY" FROM GDOSYS.GFEATURES where FEATURENAME= :vownertable;
EXEC GOOM.DblLine;
SET FEEDBACK ON
SET TIMING ON
-- ----------------------------------------------------------------------------------------