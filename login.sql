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
-- --------------------------------------------------------------------------------
-- Login Parameters for SQL Plus
-- --------------------------------------------------------------------------------
-- Set the preferred Editor
-- THis is the one I use but you can set this to anything:
--
-- DEFINE_EDITOR="C:\Program Files\JGsoft\EditPadPro6\EditPadPro.exe"
--
-- Set the sql parameters
SET DESCRIBE DEPTH 1 LINENUM OFF INDENT ON
SET ARRAYSIZE 1000
SET LINESIZE  1000
SET PAGESIZE  1000
SET LONG      10000
SET HEADING   ON
SET SERVEROUT ON
SET TIMING    ON
SET VERIFY    OFF
SET TIME      OFF
SET TERMOUT   OFF
SET TIMING    OFF
SET FEEDBACK  OFF
-- --------------------------------------------------------------------------------
--  Default formats
COLUMN TABLE_NAME   FORMAT A30
COLUMN COLUMN_NAME  FORMAT A30
COLUMN USERNAME     FORMAT A30
COLUMN TABLETYPE    FORMAT A30
COLUMN OBJECT_NAME  FORMAT A30
--
-- --------------------------------------------------------------------------------
-- Set the prompt to indicate the server and the user name.  I prefere server.user
-- but you can also use user@server.
-- --------------------------------------------------------------------------------
DEFINE v_server=&&_CONNECT_IDENTIFIER
DEFINE v_user=&&_USER
SET TERMOUT ON
SET sqlprompt "&&v_server..&&v_user. -> "
--SET sqlprompt "&&_CONNECT_IDENTIFIER..&&_USER. -> "
-- --------------------------------------------------------------------------------