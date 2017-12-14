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
-- Syntax: @CreateUser
-- Answer Prompts
-- The username and password will be the same.  Use the password command
-- to change the password after schema creation.
--
-- History: 4/07/2003  Created
--          4/11/2006  Modified for 10.2
--          7/05/2006  Modified from AddUser to allow for selected tablespace
--
-- ---------------------------------------------------------------
set timing off
set feedback off
PROMPT =========================================================================;
PROMPT Create a GeoMedia Master User account in the specified tablespace;
PROMPT .........................................................................;
ACCEPT auser CHAR    PROMPT  'Enter a user name to create ------>';
SELECT tablespace_name "AVAILABLE TABLESPACES" 
  FROM DBA_TABLESPACES 
 WHERE TABLESPACE_NAME NOT IN ('SYSTEM','UNDOTBS1','SYSAUX','TEMP','INDX','GDOSYS');
ACCEPT tabspace CHAR PROMPT 'Enter a tablespace to use (USERS) ->' DEFAULT 'USERS';
PROMPT .........................................................................;
set verify off
set termout on
set serverout on
DECLARE
c_cmdname  CONSTANT VARCHAR2(32):='CreateUser';
v_user     VARCHAR2(32):=UPPER('&auser');
v_tspace   VARCHAR2(32):=UPPER('&tabspace');
v_tabexist PLS_INTEGER:=0;
v_version  NUMBER;
BEGIN
    SELECT COUNT(1) INTO v_tabexist FROM DBA_TABLESPACES WHERE TABLESPACE_NAME = v_tspace;
    IF v_user is not NULL AND v_tabexist >0 THEN 
      SELECT NVL(SUBSTR(GOOM.GetDBVersion,1,INSTR(GOOM.GetDBVersion,'.',4)-1),0) INTO v_version from dual;
      EXECUTE IMMEDIATE 'CREATE USER '||v_user||' PROFILE "DEFAULT" IDENTIFIED BY '||v_user||' DEFAULT TABLESPACE '||v_tspace||' TEMPORARY TABLESPACE TEMP ACCOUNT UNLOCK';
      EXECUTE IMMEDIATE 'GRANT CONNECT TO '||v_user;
      EXECUTE IMMEDIATE 'GRANT RESOURCE TO '||v_user;
      EXECUTE IMMEDIATE 'GRANT CREATE TABLE TO '||v_user;
      EXECUTE IMMEDIATE 'GRANT CREATE VIEW TO '||v_user;
      EXECUTE IMMEDIATE 'GRANT CREATE SEQUENCE TO '||v_user;
      EXECUTE IMMEDIATE 'GRANT CREATE MATERIALIZED VIEW TO '||v_user;
      IF v_version > 10.1 THEN
        EXECUTE IMMEDIATE 'GRANT MERGE ANY VIEW TO '||v_user;
      END IF;
      -- Set Tablespace quotas
        EXECUTE IMMEDIATE 'ALTER USER '||v_user||' QUOTA UNLIMITED ON '||v_tspace;
      SELECT COUNT(1) INTO v_tabexist FROM DBA_TABLESPACES WHERE TABLESPACE_NAME = 'INDX';
      IF v_tabexist>0 THEN
        EXECUTE IMMEDIATE 'ALTER USER '||v_user||' QUOTA UNLIMITED ON INDX';
      END IF;
      GOOM.Response('USER CREATED', 'User '||v_user||' created in tablespace '||v_tspace||'.');
    ELSE
      GOOM.Response('USER CREATION FAILED','No user specified or invalid tablespace.');
    END IF;
    EXCEPTION
      WHEN OTHERS THEN
        GOOM.REPORT_ERROR (c_cmdname,'USER:'||v_user,'TabSpace:'||v_tspace,sqlcode,sqlerrm);
END;
/
PROMPT =========================================================================;
set verify on
set feedback on
-- ---------------------------------------------------------------