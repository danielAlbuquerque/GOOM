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
-- Creates a new master schema for use with GeoMedia/GMPro
-- Syntax: @adduser <username>
-- The username and password will be the same.  Use the password command
-- to change the password after schema creation.
--
-- History: 4/07/2003  Created
--          4/11/2006  Modified for 10.2
--
-- ---------------------------------------------------------------
set verify off
set termout on
set serverout on
DECLARE
c_proc  CONSTANT VARCHAR2(32):='ADDUSER';
v_user  VARCHAR2(32):=UPPER('&1');
v_version  NUMBER;
    BEGIN
    IF v_user is not NULL THEN 
      SELECT NVL(SUBSTR(GOOM.GetDBVersion,1,INSTR(GOOM.GetDBVersion,'.',4)-1),0) INTO v_version from dual;
      EXECUTE IMMEDIATE 'CREATE USER '||v_user||' PROFILE "DEFAULT" IDENTIFIED BY '||v_user||' DEFAULT TABLESPACE USERS TEMPORARY TABLESPACE TEMP ACCOUNT UNLOCK';
      EXECUTE IMMEDIATE 'GRANT UNLIMITED TABLESPACE TO '||v_user;
      -- These privs are required for a user that owns data.
      EXECUTE IMMEDIATE 'GRANT CONNECT TO '||v_user;
      EXECUTE IMMEDIATE 'GRANT RESOURCE TO '||v_user;
      EXECUTE IMMEDIATE 'GRANT CREATE TABLE TO '||v_user;
      EXECUTE IMMEDIATE 'GRANT CREATE VIEW TO '||v_user;
      EXECUTE IMMEDIATE 'GRANT CREATE SEQUENCE TO '||v_user;
      -- Create Mat View is optional
      EXECUTE IMMEDIATE 'GRANT CREATE MATERIALIZED VIEW TO '||v_user;
      -- Merge any view is required for 10 or later
      IF v_version > 10.1 THEN
        EXECUTE IMMEDIATE 'GRANT MERGE ANY VIEW TO '||v_user;
      END IF;
      DBMS_OUTPUT.PUT_LINE('User '||v_user||' has been created.');
    ELSE
    DBMS_OUTPUT.PUT_LINE('No User Specified');
    END IF;
    EXCEPTION
        WHEN OTHERS THEN
        DBMS_OUTPUT.PUT_LINE('Error in:'||c_proc);
        DBMS_OUTPUT.PUT_LINE('Error message: '||sqlerrm);
    END;
/
undefine 1
set verify on
-- ---------------------------------------------------------------