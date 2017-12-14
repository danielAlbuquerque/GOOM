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
-- -------------------------------------------------------------------------------------------------
-- Note:  GOOM Package must be installed before using this script.
-- History: 4/17/2007 Final
-- History: 4/06/2015 Updated
-- ----------------------------------------------------------------------------------------
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
PROMPT This script will add a new primary key column to an exisitng table.  You will be ;
PROMPT prompted to specify what to do with the existing key (if any).  If you choose to ;
PROMPT keep the old key, it will be flagged to allow nulls.  A new sequence will be     ;
PROMPT created and the new key column will be populated automatically.  GDOSYS entries  ;     
PROMPT for this table will be deleted and added back when the process is complete. Any  ;
PROMPT custom GDOSYS entries will be lost.                      ;
EXEC GOOM.DblLine;
ACCEPT tablename CHAR PROMPT 'Enter the Table Name -------------->' DEFAULT 'TESTKEY';
ACCEPT keycolnew CHAR PROMPT 'Enter the new key column (PID) ---->' DEFAULT 'PID';
ACCEPT dropflag  CHAR PROMPT 'Delete existing key? (N)/Y -------->' DEFAULT 'N';
DECLARE
  --
  v_tablename         VARCHAR2(30) :=UPPER('&tablename');
  v_keycolnew         VARCHAR2(30) :=UPPER('&keycolnew');
  v_dropflag          VARCHAR2(2)  :=UPPER('&dropflag');
  b_dropflag          BOOLEAN;
  --
--
BEGIN
    IF v_dropflag ='Y' THEN
      b_dropflag := TRUE;
    ELSE
      b_dropflag := FALSE;
    END IF;
    GOOM.AddPrimaryKey( v_tablename, v_keycolnew, b_dropflag);
END;
/
SET TIMING ON
SET FEEDBACK ON
--