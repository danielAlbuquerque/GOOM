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
-- Return the information stored in USER_SDO_GEOM_METADATA
-- ----------------------------------------------------------------------------------------
ACCEPT cuser PROMPT 'Enter the user name (USER) -->'
--
SET TERMOUT OFF
VARIABLE vuser VARCHAR2(30);
EXEC :vuser := NVL(UPPER('&cuser'),USER);
SET TERMOUT ON
COLUMN FEATURE_CLASS FORMAT A61
COLUMN DIM  FORMAT A3
COLUMN SRID FORMAT A6
COLUMN TOL  FORMAT 9.999999
BREAK ON FEATURE_CLASS SKIP 1
-- ----------------------------------------------------------------------------------------
SELECT USGM.table_name||'.'||USGM.column_name Feature_Class,NVL(TO_CHAR(SRID), 'NULL') SRID, 
       D.SDO_DIMNAME DIM, D.SDO_LB LB, D.SDO_UB UB, D.SDO_TOLERANCE TOL
  FROM ALL_SDO_GEOM_METADATA USGM, TABLE(USGM.DIMINFO) D 
 WHERE OWNER = :vuser
 ORDER BY FEATURE_CLASS, DIM;
-- ----------------------------------------------------------------------------------------