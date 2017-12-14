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
-- History:  04/17/2007  Modified.
--
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
--
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('This script will generate a reference grid for a given range');
EXEC GOOM.DblLine;
--
ACCEPT tab_name  CHAR PROMPT  'Enter the name of the ref grid table ->' Default 'REFERENCE_GRID';
ACCEPT div_x      NUM PROMPT  'Enter the # of divisions in X (10)  -->' Default 10;
ACCEPT div_y      NUM PROMPT  'Enter the # of divisions in Y (X)  --->' Default &div_x;
ACCEPT Xlowrange  NUM PROMPT  'Enter the lo range for x (0)       --->' Default 0;
ACCEPT Ylowrange  NUM PROMPT  'Enter the lo range for y (xlo)     --->' Default &Xlowrange;
ACCEPT Xhighrange NUM PROMPT  'Enter the hi range for x (100000)  --->' Default 100000;
ACCEPT Yhighrange NUM PROMPT  'Enter the hi range for y (xhi)     --->' Default &Xhighrange;
--
EXEC GOOM.DotLine;
EXEC GDODATA.GenerateReferenceGrid(UPPER('&tab_name'),&Xlowrange,&Ylowrange,&Xhighrange,&Yhighrange,&div_x,&div_y);
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
SET VERIFY ON
-- ----------------------------------------------------------------------------------------