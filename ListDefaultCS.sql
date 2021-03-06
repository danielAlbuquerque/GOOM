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
-- List the current assigned default coordinate system for this user.
-- ----------------------------------------------------------------------------------------
SET VERIFY OFF
SET TIMING OFF
SET FEEDBACK OFF
ACCEPT cuser PROMPT 'Enter the user name (USER) -->'
--
COLUMN CSGUID FORMAT A38;
COLUMN OWNER  FORMAT A30;
COLUMN NAME   FORMAT A32;
EXEC GOOM.DBLLine;
EXEC GOOM.TITLELINE('Default Coordinate System Assigned to Specified Schema');
EXEC GOOM.DotLine;
SELECT NVL(B.NAME,'UNKNOWN - Name this CS!') NAME, 
       NVL(SUBSTR(A.GPARAMETER,1,INSTR(A.GPARAMETER,'.')-1),'None Assigned') OWNER, 
       NVL(A.GVALUE,'None Assigned') CSGUID
  FROM GDOSYS.GPARAMETERS A,
       GDOSYS.GCOORDSYSTEM B
 WHERE A.GVALUE = B.CSGUID
   AND SUBSTR(A.GPARAMETER,1,INSTR(A.GPARAMETER,'.')-1)=NVL(UPPER('&cuser'),USER);
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
SET VERIFY ON
--