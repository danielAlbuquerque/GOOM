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
-- @ListCatalog
-- Decode contents of catalog table created with the create_catalog script. 
-- ----------------------------------------------------------------------------------------
-- History:
-- 06/01/2007   New List Schema Catalog Command
-- ----------------------------------------------------------------------------------------
COLUMN OBJECT FORMAT A6
COLUMN FEATURECLASS FORMAT A25
COLUMN GEOM_COL FORMAT A16
COLUMN "#ROWS" FORMAT 9999999999
COLUMN GTYPE FORMAT A5
COLUMN GDO FORMAT A14
COLUMN GM FORMAT A14
SELECT OBJ_TYPE OBJECT, FEATURECLASS, GEOM_COL, ROW_COUNT "#ROWS", OGTYPE GTYPE, SPATIAL_TYPE "SpatialType", GDO_TYPE GDO, 
       DECODE(GDO_TYPE,
       '-1','Attribute Only',
        '1','Linear        ',
        '2','Area          ',
        '3','SpatialAny    ',
        '4','Coverage      ',
        '5','Text          ',
       '10','Point         ',
       'NONE','No Metadata ',
       'Unknown'
       ) GM
FROM SCHEMA_CATALOG ORDER BY FEATURECLASS;
--