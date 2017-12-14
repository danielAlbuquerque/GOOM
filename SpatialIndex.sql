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
-- --------------------------------------------------------------------------------------------------------
-- This script spatially indexes a simgle feature class using RTREEs
-- ----------------------------------------------------------------------------------------
--
-- History:
-- 02/13/2003 Created.
-- 08/01/2003 Modified.
-- 07/02/2005 Modified to use GOOM.
-- 04/17/2007 Modified.
-- 02/06/2009 Modified to use GOOM.
-- 04/26/2010 Modified to use owner.table and binds.
--
SET SERVEROUTPUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
--
EXEC GOOM.DblLine;
EXEC GOOM.TitleLine('Create an RTREE Index on the specified feature class');
EXEC GOOM.DblLine;
ACCEPT table_name  CHAR PROMPT  'Enter the Feature Class to spatially index --->';
ACCEPT vconstraint CHAR PROMPT  'Constrain by geometry type (Y),N           --->' DEFAULT 'Y';
VARIABLE vownertable VARCHAR2(61);
VARIABLE vconsts     VARCHAR2(1);
set termout off
EXEC :vownertable := GOOM.GetOwnerObject(UPPER('&table_name'));
EXEC :vconsts     := UPPER('&vconstraint');
set termout on
EXEC GOOM.DotLine;
--
BEGIN
  IF :vconsts = 'Y' THEN
   GOOM.SpatialIndex(:vownertable,TRUE);
  ELSE
   GOOM.SpatialIndex(:vownertable,FALSE);
  END IF;
END;
/
EXEC GOOM.DblLine;
SET FEEDBACK ON
SET TIMING ON
-- ----------------------------------------------------------------------------------------