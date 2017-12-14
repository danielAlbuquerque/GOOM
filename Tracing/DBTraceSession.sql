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
--Set up tracing for a session
SET SERVEROUTPUT ON
SET TERMOUT ON
SET VERIFY OFF
SET FEEDBACK OFF
SET TIMING OFF
ALTER SYSTEM SET TIMED_STATISTICS=TRUE;
--
column USERNAME FORMAT A20;
column PROGRAM  FORMAT A20;
column TERMINAL FORMAT A12;
column SID      FORMAT 999999999;
column SERIAL   FORMAT 999999999999;
PROMPT =========================================================================;
SELECT USERNAME, 
            SID, 
        SERIAL#, 
        PROGRAM, 
         OSUSER, 
       TERMINAL 
  FROM v$session
 WHERE OSUSER NOT LIKE 'SYSTEM%';
PROMPT =========================================================================;
PROMPT ===>           SELECT A USER TO TRACE FROM THE LIST ABOVE            <===;
PROMPT =========================================================================;
ACCEPT theSID    num PROMPT 'Enter SID ---->';
ACCEPT theSERIAL num PROMPT 'Enter SERIAL ->';
PROMPT .........................................................................;
EXECUTE TraceSession(&theSID,&theSERIAL,'ON');
PROMPT Trace ends when session terminates, or run the following to end the trace:;
PROMPT .........................................................................;
EXECUTE DBMS_OUTPUT.PUT_LINE('EXECUTE TraceSession(&theSID,&theSERIAL,''OFF'');');
PROMPT =========================================================================;
--
SET FEEDBACK ON
SET TIMING ON