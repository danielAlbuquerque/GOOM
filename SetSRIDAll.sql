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
-- @SETSRIDALL
-- Set the SRID for a for all table.geometry pairs in the given schema.
-- ----------------------------------------------------------------------------------------
-- History: 12/18/2003 Intial Creation
--          01/04/2004 Modified to use GOOM package.
--          07/02/2005 Updated.
--          04/17/2007 Updated.
--          03/15/2009 Modified to use GOOM package.
--          04/26/2010 Modified to use owner.table
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET TERMOUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
--
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('Set the SRID for all feature classes in the specified schema');
EXEC GOOM.DblLine;
--
ACCEPT cuser CHAR PROMPT 'Enter the (USER) name            --> ';
ACCEPT csrid  NUM PROMPT 'Enter the SRID value (0 for NULL)--> ' DEFAULT 0;
EXEC GOOM.DotLine;
SET TERMOUT OFF
VARIABLE vuser VARCHAR2(30);
VARIABLE vsrid NUMBER;
EXEC :vuser := NVL(UPPER('&cuser'), USER);
EXEC :vsrid := &csrid;
SET TERMOUT ON
EXEC GOOM.SETSRIDALL(:vsrid, :vuser);
EXEC GOOM.DblLine;
-- ----------------------------------------------------------------------------------------
SET FEEDBACK ON
SET TIMING ON
