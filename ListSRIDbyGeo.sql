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
-- List the Geographic SRID values from CS_SRS
-- ----------------------------------------------------------------------------------------
-- History: 07/02/2005  Created.
--          04/17/2007  Modified.
-- ----------------------------------------------------------------------------------------
PROMPT ==============================================================================;
PROMPT List the GeoMgraphic SRID claues from CS_SRS;
PROMPT ==============================================================================;
Column NAME format A60;
column SRID format 999999999999999999999;
SELECT A.CS_NAME NAME,A.SRID 
  FROM MDSYS.CS_SRS A, MDSYS.GEODETIC_SRIDS B
 WHERE A.SRID=B.SRID ORDER BY A.SRID;
PROMPT ==============================================================================;