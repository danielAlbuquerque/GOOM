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
-- List Domain (Spatial) Indexes and their Status
-- ----------------------------------------------------------------------------------------
SET VERIFY OFF
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
ACCEPT cuser PROMPT 'Enter the user name (USER) ->';
EXEC GOOM.DotLine;
COLUMN TYPE FORMAT A6
COLUMN STATUS FORMAT A6
COLUMN INDEX_TABLE FORMAT A12
COLUMN TABLE_NAME FORMAT A30
COLUMN COLUMN_NAME FORMAT A30
COLUMN INDEX_NAME FORMAT A30
COLUMN COMMAND FORMAT A60
Select Table_name,
       Column_name, 
       Index_name, 
       SDO_INDEX_TYPE TYPE, 
       SDO_INDEX_TABLE INDEX_TABLE, 
       SDO_INDEX_STATUS STATUS,
       'DROP INDEX '||INDEX_NAME||' FORCE ;' COMMAND
  from ALL_SDO_INDEX_INFO
 WHERE SDO_INDEX_OWNER=NVL('&cuser', USER);
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
SET VERIFY ON
-- ----------------------------------------------------------------------------------------