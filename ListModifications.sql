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
-- @ListModifications
-- List Modifications from the GDOSYS.ModificationLog table.
-- ----------------------------------------------------------------------------------------
-- History: 04/10/2011 Created
-- ----------------------------------------------------------------------------------------
SET VERIFY OFF
SET TIMING OFF
SET FEEDBACK OFF
--
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('List latest modifications in the GDOSYS.MODIFICATIONLOG');
EXEC GOOM.DblLine;
ACCEPT NUM PROMPT 'List past N modifications (5) -> ' DEFAULT 5;
EXEC GOOM.DotLine;
COLUMN FEAT_CLASS  FORMAT A61;
COLUMN MOD_TYPE    FORMAT A8;
COLUMN KEY_VALUE   FORMAT A15;
--
SELECT A.TABLENAME FEAT_CLASS, DECODE(B.TYPE,1,'INSERT',2,'UPDATE',3,'DELETE') MOD_TYPE, TO_CHAR(B.MODIFIEDDATE, 'mm/dd/yyyy hh24:mi:ss') MOD_DATE, TO_CHAR(B.KEYVALUE1) KEY_VALUE
  FROM GDOSYS.MODIFIEDTABLES A, GDOSYS.MODIFICATIONLOG B
 WHERE A.MODIFIEDTABLEID=B.MODIFIEDTABLEID 
   AND b.modificationnumber > (SELECT MAX(modificationnumber)-&NUM FROM GDOSYS.MODIFICATIONLOG);
EXEC GOOM.DblLine;
--
SET FEEDBACK ON
SET TERMOUT ON
SET VERIFY ON
SET TIMING ON
-- ----------------------------------------------------------------------------------------