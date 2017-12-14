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
-- @MBR_COPY  -- follow the prompts.
-- Copy MBR values (from USER_SDO_GEOM_METADATA) from one feature class to another.
-- ----------------------------------------------------------------------------------------
-- History: 10/01/2003  Created.
--          04/10/2005  Modified.
--          07/02/2005  Modified.
--          04/17/2007  Modified.
--          03/06/2009  Modified to use GOOM.
--          04/26/2010  Modified.
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
--
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('Copy USER_SDO_GEOM_METADATA MBR values from one feature class to another');
EXEC GOOM.DblLine;
ACCEPT source_layer char PROMPT 'Enter Source Feature Class --->' DEFAULT 'CITIES';
COLUMN "AVAILABLE GEOMETRY COLUMNS" FORMAT A26;
SELECT column_name "AVAILABLE GEOMETRY COLUMNS" FROM COLS WHERE DATA_TYPE='SDO_GEOMETRY' AND TABLE_NAME=UPPER('&source_layer');
PROMPT ;
ACCEPT source_geom char PROMPT  'Enter Source Geometry Column ->' DEFAULT 'GEOMETRY';
EXEC GOOM.DotLine;
ACCEPT target_layer char PROMPT 'Enter Target Feature Class --->' DEFAULT 'CITIES';
COLUMN "AVAILABLE GEOMETRY COLUMNS" FORMAT A26;
SELECT column_name "AVAILABLE GEOMETRY COLUMNS" FROM COLS WHERE DATA_TYPE='SDO_GEOMETRY' AND TABLE_NAME=UPPER('&target_layer');
PROMPT ;
ACCEPT target_geom char PROMPT  'Enter Target Geometry Column ->' DEFAULT 'GEOMETRY';
EXEC GOOM.DotLine;
--
EXEC GOOM.CopyMBR(UPPER('&source_layer'), UPPER('&source_geom'), UPPER('&target_layer'), UPPER('&target_geom'));
--
EXEC GOOM.DblLine;
SET FEEDBACK ON
SET TIMING ON
-- ----------------------------------------------------------------------------------------