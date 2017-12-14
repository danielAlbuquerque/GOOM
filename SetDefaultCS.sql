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
-- @SetDefaultCS
-- Set the Default Coordinate System to use in this schema.  Lists exiting CS's and
-- allows user to pick the one to use.
-- ----------------------------------------------------------------------------------------
-- History:  07/02/2005  Created.
--           04/17/2007  Updated.
--           03/06/2009  Update to use GOOM.
--           04/26/2010  Supports specifying schema.
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET TERMOUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
--
COLUMN CSGUID FORMAT A38;
COLUMN OWNER  FORMAT A30;
COLUMN NAME   FORMAT A40;
--
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('Set the default coordinate system for the specified schema');
EXEC GOOM.DblLine;
--
ACCEPT cuser CHAR PROMPT 'Enter the (USER) name            --> ';
VARIABLE vowner    VARCHAR2(30);
VARIABLE vcsguid   VARCHAR2(38);
EXEC :vowner := NVL(UPPER('&cuser'),USER);
EXEC GOOM.DotLine;
EXEC GOOM.TitleLine('Current Default Coordinate System in '||:vowner);
EXEC GOOM.DotLine;
SELECT NVL(B.NAME,'UNKNOWN - Name this CS!') NAME, 
       NVL(A.GVALUE,'None Assigned') CSGUID
  FROM GDOSYS.GPARAMETERS A,
       GDOSYS.GCOORDSYSTEM B
 WHERE A.GVALUE = B.CSGUID
   AND SUBSTR(A.GPARAMETER,1,INSTR(A.GPARAMETER,'.')-1)=:vowner;
EXEC GOOM.DotLine;
EXEC GOOM.TitleLine('Coordinate Systems in GDOSYS.GCoordSystem');
EXEC GOOM.DotLine;
SELECT NVL(NAME,'*Not Assigned a Name*') NAME, CSGUID from GDOSYS.GCOORDSYSTEM ORDER BY NAME;
EXEC GOOM.DotLine;
ACCEPT csguid CHAR PROMPT 'Enter a CSGUID that you want to assign as default for this user ->';
EXEC :vcsguid := RTRIM('&csguid');
--
EXEC GOOM.SetDefaultCS(:vcsguid, :vowner);
EXEC GOOM.DblLine;
-- ----------------------------------------------------------------------------------------
SET FEEDBACK ON
SET TIMING ON

-- ----------------------------------------------------------------------------------------