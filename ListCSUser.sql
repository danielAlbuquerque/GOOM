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
-- List the current coordinate system assignments and the default CS
--
SET SERVEROUTPUT ON
SET TERMOUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
--
COLUMN CSGUID FORMAT A38;
COLUMN OWNER  FORMAT A30;
COLUMN NAME   FORMAT A40;
COLUMN FEATURE    FORMAT A34
COLUMN CSNAME     FORMAT A40
--
ACCEPT cuser CHAR PROMPT 'Enter the (USER) name  --> ';
--
VARIABLE vowner    VARCHAR2(30);
EXEC :vowner := NVL(UPPER('&cuser'),USER);
--
EXEC GOOM.DBLLine;
EXEC GOOM.TitleLine('Coordinate System currently defined as default for '|| :vowner);
EXEC GOOM.DBLLine;
SELECT CS.NAME CSNAME, GP.GVALUE CSGUID FROM GDOSYS.GCOORDSYSTEM CS, GDOSYS.GPARAMETERS GP  
 WHERE GOOM.SplitOwnerObject(GP.GPARAMETER,'OWNER') = USER
   AND GP.GVALUE=CS.CSGUID;
EXEC GOOM.TitleBlock('Coordinate System Assignments on a Feature Class Basis');
SELECT a.FEATURENAME FEATURE, b.NAME CSNAME, b.CSGUID CSGUID
  FROM GDOSYS.FIELDLOOKUP a, GDOSYS.GCOORDSYSTEM b, GDOSYS.GEOMETRYPROPERTIES c , GDOSYS.GFEATURES d
 WHERE a.INDEXID = c.INDEXID 
   AND b.CSGUID = c.GCOORDSYSTEMGUID 
   AND a.FEATURENAME = d.FEATURENAME order by a.featurename;
EXEC GOOM.DblLine;
set feedback on
set timing on
