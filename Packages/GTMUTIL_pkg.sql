SET SERVEROUTPUT ON
-- ------------------------------------------------------------------------------------------------------------------
-- @GTMUTIL_PKG
-- GTM Utility Package
-- Helpful additions to GeoMedia's Transaction Manager package - GTM.
-- ------------------------------------------------------------------------------------------------------------------
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
-- @GTMUTIL_PKG
-- GeoMedia Transaction Utility (GTMUTIL) Package for 6.1.0, 6.1.1, and 2013 or later.
-- Oracle Object Model Procedures and Functions for use with GeoMedia Transaction Manager.  This 
-- package provides extensions for the GTM Package which is delivered with GeoMedia's Transaction
-- Manager application.   The GTM package must be installed before installing this package as it
-- is required.  The GOOM package is also required.
--
-- Special thanks to Bruce Hall who helped make this package what it is.
--
-- Run script as system DBA after the creation of the GDOSYS metadata schema, and after first 
-- installing both the GOOM and GTM packages.
--
-- Any user account that uses this package must be explicitly granted the following privileges:
-- GRANT CREATE TABLE TO <user>;
-- GRANT CREATE SEQUENCE TO <user>;
-- These privileges are required by Oracle when manipulating spatial (domain based) indexes.
--
-- ----------------------------------------------------------------------------------------
--
-- Release History:
-- 04/18/2007  Released to WEB.
-- 03/21/2008  Released to WEB.
-- 04/30/2009  Released to WEB.
-- 12/03/2009  Released to WEB.
-- 04/30/2010  Released to WEB.
-- 03/15/2012  Released to WEB.
-- 07/15/2013  Released to WEB.
-- 07/15/2014  Released to WEB.
-- 07/15/2015  Released to WEB.
-- 07/15/2016  Released to WEB.
--
-- History: 
-- 07/06/2016  Minor updates for 2016 release.
-- 
-- ----------------------------------------------------------------------------------------
PROMPT ******************************************************************;
PROMPT **                    GTMUTIL PACKAGE 2016                      **;
PROMPT **      GeoMedia Transaction Utility Package Installation       **;
PROMPT **                                                              **;
PROMPT ** This package provides various utility functions and          **;
PROMPT ** procedures for use with GeoMedia Transaction Manager and     **;
PROMPT ** Oracle''s Workspace Manager.  It is a supplement to the GTM  **;
PROMPT ** package provided with GeoMedia''s Transaction Manager        **;
PROMPT **..............................................................**;
PROMPT **                  INSTALLATION INFORMATION                    **;
PROMPT ** -> Oracle 11G or later is REQUIRED.                          **;
PROMPT ** -> Installation requires a DBA connection.                   **;
PROMPT ** -> The default installation location is the GDOSYS schema.   **;
PROMPT ** -> The GOOM and GTM packages must be installed first.        **;
PROMPT **..............................................................**;
-- --------------------------------------------------------------------------
-- --------------------------------------------------------------------------
CREATE OR REPLACE PACKAGE GDOSYS.GTMUTIL AUTHID CURRENT_USER IS
-- --------------------------------------------------------------------------
-- Misc Procedures and Functions
-- --------------------------------------------------------------------------

-- Return the package version
PROCEDURE VERSION;
-- Return online help
PROCEDURE HELPME;
-- Return Workspace manager information
PROCEDURE WMINFO;
-- Return the GTM metadata version.
FUNCTION  GetGTMVersion RETURN NUMBER;

-- --------------------------------------------------------------------------
-- Revision Set Operations
-- --------------------------------------------------------------------------

-- Set the active workspace to LIVE
PROCEDURE SetLive;
-- Set the active workspace to the parent of the specified child.
PROCEDURE SetParent( v_childrev IN VARCHAR2);
-- Output the name of the active revision set.
PROCEDURE GetActive;
-- List all revision sets for specified user.
PROCEDURE ListRevSets( v_schema  IN VARCHAR2 DEFAULT USER);
-- List all revision sets in database instance.
PROCEDURE ListAllRevSets;
-- List all child revision sets for specified parent.  LIVE is the default.
PROCEDURE ListChildRevSets( v_revsetname IN VARCHAR2 DEFAULT 'LIVE');
-- Force deletion of the specified revision set.
PROCEDURE DBADeleteRevSet( v_revsetname  IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER);
-- Retire all revision sets owned by the user running procedure.
PROCEDURE RetireAllRevSets;
-- Retire all revision sets currently in use for specified owner, if NULL, reture ALL.
PROCEDURE DBADeleteAllRevSets( v_schema   IN VARCHAR2 DEFAULT NULL);
-- Automatically generate a specified number of revision sets.
PROCEDURE CreateMultiRevSets( i_num      IN INTEGER,  b_opt   IN BOOLEAN  DEFAULT FALSE);
-- Return the name of the parent for the specified revision set.
FUNCTION  GetParentRevSet( v_revsetname  IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER) 
          RETURN VARCHAR2;
-- Return an automatically generated revision set name.
FUNCTION  GenerateRevSetName 
          RETURN VARCHAR2;

-- --------------------------------------------------------------------------
-- Secured Table Operations
-- --------------------------------------------------------------------------

-- Output a list af all version enabled tables in the specified schema.
PROCEDURE ListSecuredTables( v_schema IN VARCHAR2 DEFAULT USER);

-- --------------------------------------------------------------------------
-- Spatial Indexing and Tuning
-- --------------------------------------------------------------------------

-- Create  a spatial index on a version enabled table.
PROCEDURE GTMSpatialIndex( v_tablename IN VARCHAR2, 
                                 b_opt IN BOOLEAN  DEFAULT TRUE);
-- Create a spatial index on all version enabled tables in the schema.
PROCEDURE GTMSpatialIndexAll( v_schema  IN VARCHAR2 DEFAULT USER, 
                                b_opt  IN BOOLEAN  DEFAULT TRUE);
-- Create statistics on all version enabled tables in the schema.
PROCEDURE AnalyzeLT_TABLES( v_schema    IN VARCHAR2 DEFAULT USER);

-- --------------------------------------------------------------------------
-- Privileges
-- --------------------------------------------------------------------------

-- Grant DML privileges on all the objects in the schema.
PROCEDURE GrantPrivOnSchema( v_grantee IN VARCHAR2, 
                              v_access IN VARCHAR2 DEFAULT 'R', 
                                 v_ddl IN VARCHAR2 DEFAULT 'N');
-- Grant DML privileges on a specific version enabled table.
PROCEDURE GrantPrivOnTable( v_grantee  IN VARCHAR2, 
                                 v_fc  IN VARCHAR2, 
                             v_access  IN VARCHAR2 DEFAULT 'R', 
                                v_ddl  IN VARCHAR2 DEFAULT 'N');

-- --------------------------------------------------------------------------
-- Schema Operations
-- --------------------------------------------------------------------------

-- Disable all triggers in a schema.
PROCEDURE DisableAllTriggers( v_schema IN VARCHAR2 DEFAULT USER);
-- Enable all triggers in a schema.
PROCEDURE EnableAllTriggers ( v_schema IN VARCHAR2 DEFAULT USER);
-- Verify a schema prior to version enabling the tables.
PROCEDURE VerifySchema( v_schema IN VARCHAR2 DEFAULT USER);
-- Drop a GTM user account.  
PROCEDURE DropGTMUser ( v_username in VARCHAR2);

-- --------------------------------------------------------------------------
-- Valid Time Operations
-- --------------------------------------------------------------------------

-- Return the DML count for VT operations.
FUNCTION GetVTInsertCount( v_tablename IN VARCHAR2) RETURN INTEGER;
FUNCTION GetVTUpdateCount( v_tablename IN VARCHAR2) RETURN INTEGER;
FUNCTION GetVTDeleteCount( v_tablename IN VARCHAR2) RETURN INTEGER;
--
END;
/
show errors

-- -----------------------------------------------------------------------
-- GTMUTIL Package Body
-- -----------------------------------------------------------------------

CREATE OR REPLACE PACKAGE BODY GDOSYS.GTMUTIL IS

-- --------------------------------------------------------------------------
-- Internal cursors
-- --------------------------------------------------------------------------

-- Get the list of Workspaces/Revision Sets by owner.
CURSOR GetRevSets ( v_schema VARCHAR2 DEFAULT USER) IS 
       SELECT WORKSPACE, PARENT_WORKSPACE, CREATETIME 
         FROM ALL_WORKSPACES 
        WHERE OWNER = v_schema
          AND WORKSPACE<>'LIVE'
     ORDER BY WORKSPACE,PARENT_WORKSPACE;
-- Get the list of all Workspaces/Revision Sets.
CURSOR GetAllRevSets IS 
       SELECT OWNER, WORKSPACE, PARENT_WORKSPACE, CREATETIME 
         FROM ALL_WORKSPACES 
        WHERE WORKSPACE<>'LIVE'
     ORDER BY WORKSPACE,PARENT_WORKSPACE;
-- Getlist of all top level (parent workspaces) by user.
CURSOR GetTopRevSets (v_schema VARCHAR2 DEFAULT USER) IS 
       SELECT OWNER, WORKSPACE, CREATETIME 
         FROM ALL_WORKSPACES 
        WHERE OWNER = v_schema
          AND WORKSPACE <> 'LIVE'
          AND PARENT_WORKSPACE = 'LIVE';
-- Get the list of all level (parent workspaces).
CURSOR GetAllTopRevSets IS 
       SELECT OWNER, WORKSPACE, CREATETIME 
         FROM ALL_WORKSPACES 
        WHERE WORKSPACE <> 'LIVE'
          AND PARENT_WORKSPACE = 'LIVE';
-- Return all version enabled feature classes
CURSOR GetSecureTableName ( v_schema VARCHAR2 DEFAULT USER) IS
       SELECT b.LTT_VIEW_NAME, a.column_name 
         FROM all_tab_columns a, gdosys.ltt_tables b
        WHERE owner = v_schema
          AND a.table_name = b.LTT_VIEW_NAME||'_LT'
          AND a.data_type='SDO_GEOMETRY';
-- Return the state of all version enabled tables in the specified schema.
CURSOR GetSecuredTableType(v_schema VARCHAR2 DEFAULT USER) IS    
       Select TABLE_NAME, HISTORY, 
       DECODE (STATE,'VERSIONED'    ,'READY - Version-enabled.',
                     'DV'           ,'BUSY  - The table is being versioned disabled',
                     'EV'           ,'BUSY  - The table is being versioned enabled',
                     'DDL'          ,'DDL   - The table is in an active DDL session',
                     'BDDL'         ,'BDDL  - BeginDDL is running on this table.',
                     'CDDL'         ,'CDDL  - CommitDDL is running on this table.',
                     'LWDV'         ,'LWDV  - BAD STATE! Must fix before using.',
                     'LWEV'         ,'LWEV  - BAD STATE! Must fix before using.',
                     'LW_DISABLED'  ,'LW_D  - BAD STATE! Must fix before using.',
                     'UNKNOWN') STATE, CONFLICT
         FROM ALL_WM_VERSIONED_TABLES
        WHERE OWNER = v_schema;
-- Return the name of all triggers in the specified schema.
CURSOR GetTriggerName(v_user VARCHAR2 DEFAULT USER) IS 
       SELECT TABLE_NAME, TRIGGER_NAME FROM ALL_TRIGGERS WHERE OWNER=v_user;
-- Return a list of child revision sets for the specifed owner.
CURSOR GetChildRevSets ( v_revsetname VARCHAR2 DEFAULT 'LIVE') IS 
       SELECT OWNER, WORKSPACE, CREATETIME 
         FROM ALL_WORKSPACES 
        WHERE PARENT_WORKSPACE = v_revsetname
     ORDER BY OWNER, WORKSPACE;

-- -----------------------------------------------------------------------
-- Internal variables
  c_pkg_version  VARCHAR2(12):='2016.002';    -- Current package version
  c_pkg_date     VARCHAR2(12):='07/29/2016';  -- Current package date
-- -----------------------------------------------------------------------
-- Global ERROR and EXCEPTION Handling
-- -----------------------------------------------------------------------
  e_TableNotFound       EXCEPTION;
  e_TableIsView         EXCEPTION;
  e_TabOwnerError       EXCEPTION;
  e_GeometryNotFound    EXCEPTION;
  e_SequenceNotFound    EXCEPTION;
  e_NoDataFound         EXCEPTION;
  e_NoMetadataAccess    EXCEPTION;
  e_NoMetadataFound     EXCEPTION;
  e_NoPrivilege         EXCEPTION;
  e_InvalidDimension    EXCEPTION;
  e_TabSpaceNotFound    EXCEPTION;
  e_CannotBeNull        EXCEPTION;
  PRAGMA EXCEPTION_INIT(e_TableNotFound, -942);
  PRAGMA EXCEPTION_INIT(e_NoDataFound,    100);
  PRAGMA EXCEPTION_INIT(e_NoPrivilege,  -1031);
  --Exception error msgs
  c_msgTableNotFound    CONSTANT VARCHAR2(70) := 'The specified table or view could not be found: ';
  c_msgTableExists      CONSTANT VARCHAR2(70) := 'The specified table or view already exists: ';
  c_msgTableIsView      CONSTANT VARCHAR2(70) := 'A view has been specified - no processing required: ';
  c_msgTabOwnerError    CONSTANT VARCHAR2(70) := 'Only the owner of the table is allowed to do this: ';
  c_msgGeometryNotFound CONSTANT VARCHAR2(70) := 'Specified geometry column could not be found: ';
  c_msgSequenceNotFound CONSTANT VARCHAR2(70) := 'Specified sequence could not be found: ';
  c_msgNoDataFound      CONSTANT VARCHAR2(70) := 'Specified data could not be found: ';
  c_msgNoMetadataAccess CONSTANT VARCHAR2(70) := 'No insert/delete privileges on MDSYS.SDO_GEOM_METADATA_TABLE: ';
  c_msgNoMetadataFound  CONSTANT VARCHAR2(70) := 'No metadata entries found in ALL_GEOM_METADATA_TABLE for: ';
  c_msgNoPrivilege      CONSTANT VARCHAR2(70) := 'User does not have privilege for this operation.';
  c_msgInvalidDimension CONSTANT VARCHAR2(70) := 'The geometry dimensions are not valid for this operation.';
  c_msgTabSpaceNotFound CONSTANT VARCHAR2(70) := 'Specified tablespace does not exist: ';
  c_msgCannotBeNull     CONSTANT VARCHAR2(70) := 'Input Parameters Cannot be Null: ';
  c_msgIndexingFailed   CONSTANT VARCHAR2(70) := 'Spatial Indexing failed for: ';
  -- Other standard msgs 
  c_msgNoKeyColumn      CONSTANT VARCHAR2(25) := 'No key column assigned.';
  c_msgOraError         CONSTANT VARCHAR2(25) := 'Oracle Error: ORA';
  c_msgStart            CONSTANT VARCHAR2(22) := 'Process Started for: ';
  c_msgComplete         CONSTANT VARCHAR2(22) := 'Process Complete for: ';
  c_msgGoomInternal     CONSTANT VARCHAR2(38) := 'GOOM - An Internal Error has occurred.';
  c_msgSRIDIsNull       CONSTANT VARCHAR2(46) := 'NULL SRID indicated - OK for projected data: ';
  c_msgNoSIDX           CONSTANT VARCHAR2(42) := 'The table does NOT have a spatial index: ';
  -- Procedure Exception Responses
  c_msgError            CONSTANT VARCHAR2(10) := ' - ERROR';
  c_msgWarning          CONSTANT VARCHAR2(10) := ' - WARNING';
  c_msgVerify           CONSTANT VARCHAR2(10) := ' - VERIFY';
  c_msgInform           CONSTANT VARCHAR2(10) := ' - INFORM';

-- -----------------------------------------------------------------------
-- Misc Procedures and Functions
-- -----------------------------------------------------------------------

-- Return version and date of this package.
-- Syntax: EXEC GTMUTIL.VERSION;
  PROCEDURE VERSION IS
    v_indexspace  VARCHAR2(30);
    c_support     CONSTANT VARCHAR2(14) := 'Via Email Only';
    c_emailpath   CONSTANT VARCHAR2(29) := 'chuck.woodbury@intergraph.com';
    c_onlinehelp  CONSTANT VARCHAR2(20) := 'EXEC GTMUTIL.HELPME';
    i_pad         PLS_INTEGER           :=66;
  BEGIN
    v_indexspace:=GOOM.GetGOOMIndexSpace;
    GOOM.DblLine(i_pad);
    GOOM.TitleLine('INTERGRAPH',i_pad,'**');
    GOOM.TitleLine('GTM Utility Package 2016',i_pad,'**');
    GOOM.DblLine(i_pad);
    GOOM.TitleLine('Author: Chuck Woodbury, Senior Technical Consultant', i_pad,'**');
    GOOM.TitleLine('Hexagon Geospatial Division', i_pad,'**');
    GOOM.TitleLine('Intergraph Technology Services', i_pad,'**');
    GOOM.DblLine(i_pad);
    GOOM.Response('Version',c_pkg_version,i_pad,TRUE);
    GOOM.Response('Date',c_pkg_date,i_pad,TRUE);
    GOOM.Response('Spatial Index Tablespace',v_indexspace,i_pad,TRUE);
    GOOM.Response('Bug Reports and Support',c_support,i_pad,TRUE);
    GOOM.Response('Email',c_emailpath,i_pad,TRUE);
    GOOM.Response('Online Help',c_onlinehelp,i_pad,TRUE);
    GOOM.DblLine(i_pad);
  END VERSION;

-- Online Help
-- Syntax: EXEC GTMUTIL.HELPME;
  PROCEDURE HELPME IS
    BEGIN
    GOOM.DblLine;
    GOOM.TitleLine('GTMUTIL PACKAGE PROCEDURES SYNTAX   -   GTM UTILITIES');
    GOOM.DblLine;
    GOOM.TitleLine('MISC Procedures:');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GTMUTIL.VERSION;                       -- GTMUTIL Version Info');
    GOOM.DBMSG('EXEC GTM.GetVersion;                        -- GTM Package Version Info');
    GOOM.DBMSG('EXEC GTMUTIL.WMINFO;                        -- Workspace Manager Settings');
    GOOM.DBMSG('EXEC GTMUTIL.VerifySchema;                  -- Verify Schema prior to securing, Schema must pass all tests.');
    GOOM.DBMSG('Select GTMUTIL.GetGTMVersion from dual;     -- GTM Metadata Version');
    GOOM.DBMSG('EXEC GTMUTIL.ListSecuredTables;             -- List Secured Tables and Status');
    GOOM.DotLine;
    GOOM.TitleLine('Revision Set Procedures');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GTMUTIL.SetLive;                        -- Set Active Revision Set to LIVE');
    GOOM.DBMSG('EXEC GTMUTIL.SetParent(c_child);             -- Set Active Revision Set to Parent of Child Rev Set');
    GOOM.DBMSG('EXEC GTMUTIL.ListRevSets([c_user]);          -- List Revision Sets in User');
    GOOM.DBMSG('EXEC GTMUTIL.GetActive;                      -- List Current Active Rev Set');    
    GOOM.DBMSG('EXEC GTMUTIL.ListAllRevSets;                 -- List All Accessible Revision Sets');
    GOOM.DBMSG('EXEC GTMUTIL.RetireAllRevSets;               -- Retire All User Revsets Sets');
    GOOM.DBMSG('--');
    GOOM.DBMSG('-- GTM Package procedures......................................................');
    GOOM.DBMSG('EXEC GTM.CreateRevisionSet( v_RevisionSetName );     -- Create a new revision set');                          
    GOOM.DBMSG('EXEC GTM.RetireRevisionSet( v_RevisionSetName );     -- Retire a revision set');
    GOOM.DBMSG('EXEC GTM.RefreshRevisionSet ( v_RevisionSetName );   -- Update a revision set');
    GOOM.DBMSG('EXEC GTM.SetActiveRevisionSet( v_RevisionSetName );  -- Activate a revision set'); 
    GOOM.DBMSG('EXEC GTM.CommitRevisionSet( v_RevisionSetName );     -- Commit the revision set');
    GOOM.DBMSG('EXEC GTM.DiscardRevisionSet( v_RevisionSetName );    -- Discard all edits in revision set');         
    GOOM.DBMSG('EXEC GTM.LockRevisionSet( v_RevisionSetName );       -- Lock the revision set'); 
    GOOM.DBMSG('SELECT GTM.GetActiveRevisionSet FROM DUAL;           -- Get the active revision set and/or parent');
    GOOM.DBMSG('SELECT GTM.GetRevisionSetParent( v_RevisionSetName ) FROM DUAL');
    GOOM.DBMSG('--');
    GOOM.DBMSG('Select GTMUTIL.GenerateRevSetName from dual  -- Generate Rev Set Name: VARCHAR2(30)');
    GOOM.DBMSG('-- Create i_num revision sets, default Optimistic = TRUE, Pessimistic=FALSE');
    GOOM.DBMSG('EXEC GTMUTIL.CreateMultiRevSets(i_num, [b_optimistic]);');
    GOOM.DBMSG('-- For DBA''s Only:');
    GOOM.DBMSG('EXEC GTMUTIL.DBADeleteRevSet([c_revsetname],[c_user]);  -- Delete named revision set in specified user.');
    GOOM.DBMSG('EXEC GTMUTIL.DBADeleteAllRevSets([c_user]);             -- Delete All Revision Sets in User, Pass NULL for all users.');
    GOOM.DotLine;
    GOOM.TitleLine('Create Spatial Index on Secured Tables');   
    GOOM.DotLine;
    GOOM.DBMSG('-- Index a single table');
    GOOM.DBMSG('EXEC GTMUTIL.GTMSpatialIndex(c_tablename, b_opt)');
    GOOM.DBMSG('c_tablename - the tablename.');
    GOOM.DBMSG('b_opt       - Default TRUE - Restrict by GTYPE.');
    GOOM.DBMSG('-- Index all securred tables');
    GOOM.DBMSG('EXEC GTMUTIL.GTMSpatialIndexAll(b_opt)');
    GOOM.DotLine(10);
    GOOM.DBMSG('-- Analyze Stats (DBA Only) for schema.');
    GOOM.DBMSG('EXEC GTMUTIL.AnalyzeLT_TABLES(c_owner)');
    GOOM.DBMSG('c_owner     - the owner of the table.');
    GOOM.DotLine;
    GOOM.TitleLine('Secure Table Privileges');   
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GTMUTIL.GrantPrivOnSchema(c_grantee, c_access, c_ddl);');
    GOOM.DBMSG('c_grantee - the user/role receiving the privilege.');
    GOOM.DBMSG('c_access  - ''R'' for read-only (default), ''W'' for read write.');
    GOOM.DBMSG('c_ddl     - ''N'' to run (default), ''Y'' to generate DDL statements.');
    GOOM.DotLine(10);
    GOOM.DBMSG('EXEC GTMUTIL.GrantPrivOnTable(c_grantee, c_fc, c_access, c_ddl);');
    GOOM.DBMSG('c_grantee - the user/role receiving the privilege.');
    GOOM.DBMSG('c_fc      - the feature class getting the privilege.');
    GOOM.DBMSG('c_access  - ''R'' for read-only (default), ''W'' for read write.');
    GOOM.DBMSG('c_ddl     - ''N'' to run (default), ''Y'' to generate DDL statements.');
    GOOM.DotLine;
    GOOM.TitleLine('Other Tools'); 
    GOOM.DotLine;
    GOOM.DBMSG('-- Disable triggers prior to securing, enable afterward.');
    GOOM.DBMSG('EXEC GTMUTIL.DisableAllTriggers;  -- Disables all triggers prior to securing.');  
    GOOM.DBMSG('EXEC GTMUTIL.EnableAllTriggers;   -- Enables all triggers after securing.');
    GOOM.DBMSG('-- Drop schema containing secured tables (DBA Only)');
    GOOM.DBMSG('EXEC GTMUTIL.DropGTMUser(c_username);');
    GOOM.DotLine;
    GOOM.TitleLine('Valid Time Procedures');
    GOOM.DotLine;
    GOOM.DBMSG('i_count:= GTMUTIL.GetVTInsertCount(c_tablename);  -- Return Valid Time DML Counts');
    GOOM.DBMSG('i_count:= GTMUTIL.GetVTUpdateCount(c_tablename);  -- Return Valid Time DML Counts');
    GOOM.DBMSG('i_count:= GTMUTIL.GetVTDeleteCount(c_tablename);  -- Return Valid Time DML Counts');
    GOOM.DBMSG('--');
    GOOM.DBMSG('-- GTM Package procedures.........................');
    GOOM.DBMSG('EXEC GTM.SetActiveDate( d_TemporalDate );            -- Set the valid time active date');
    GOOM.DBMSG('SELECT GTM.GetActiveDate from DUAL;                  -- Return the curent active date');
    GOOM.DBMSG('SELECT GTM.IsTableSecure( v_TableName ) from DUAL;   -- Boolean check to see if table is secure');   
    GOOM.DBMSG('EXEC GTM.SetValidTime( d_ValidFrom, d_ValidUntil );  -- Set the Valid Time to and from');
    GOOM.DBMSG('SELECT GTM.GetValidFrom from DUAL;                   -- Get the current ValidFrom date');
    GOOM.DBMSG('SELECT GTM.GetValidUntil from DUAL;                  -- Set the current ValidTill date');
    GOOM.DBMSG('EXEC GTM.SetValidTimeCaptureMode( i_CaptureMode);    -- Set the VT capture mode');
    GOOM.DBMSG('SELECT GTM.GetValidTimeCaptureMode from DUAL;        -- Get the current VT capture mode');
    GOOM.DotLine;
    GOOM.DblLine;
  END HELPME;
 
-- Workspace Manager Information
-- Syntax: EXEC GTMUTIL.WMINFO;
  PROCEDURE WMINFO IS
    i_cur       PLS_INTEGER;
    v_sql       VARCHAR2(255);
    v_name      VARCHAR2(64);
    v_value     VARCHAR2(64);
    i_dum       PLS_INTEGER;
  BEGIN
    GOOM.titleBlock('CURRENT WORKSPACE MANAGER PARAMETERS',68);
    v_sql := 'SELECT NAME,VALUE FROM WM_INSTALLATION';
    i_cur := DBMS_SQL.OPEN_CURSOR;
    DBMS_SQL.PARSE( i_cur, v_sql, DBMS_SQL.NATIVE);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 1, v_name,64);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 2, v_value,64);
    i_dum := DBMS_SQL.EXECUTE(i_cur);
    LOOP
      IF (DBMS_SQL.FETCH_ROWS(i_cur) = 0) THEN
        EXIT;
      END IF;
      DBMS_SQL.COLUMN_VALUE(i_cur, 1, v_name);
      DBMS_SQL.COLUMN_VALUE(i_cur, 2, v_value);
      GOOM.RESPONSE(v_name,v_value,68,TRUE);
    END LOOP;
    DBMS_SQL.CLOSE_CURSOR( i_cur );
    GOOM.DashLine(68);
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
  END WMINFO;
--
-- GetGTMVersion returns the current version of GTM metadata in GDOSYS.
-- Syntax: SELECT GTMUTIL.GetGTMVersion FROM DUAL;
  FUNCTION GetGTMVersion RETURN NUMBER IS
    c_cmdname     CONSTANT VARCHAR2(14) := 'GetGTMVersion';
    n_version     NUMBER                := 0.0;
    v_sql         VARCHAR2(255);
  BEGIN
    IF NOT GOOM.chkTable('GDOSYS.GPARAMETERS') THEN
      RAISE e_TableNotFound;
    END IF;
    v_sql:='SELECT GVALUE FROM GDOSYS.GPARAMETERS WHERE GPARAMETER = ''GTMVersion''';
    EXECUTE IMMEDIATE v_sql INTO n_version;
    RETURN n_version;
  EXCEPTION
    WHEN e_TableNotFound THEN
      GOOM.Response( c_cmdname || c_msgError, c_msgTableNotFound || 'GDOSYS.GPARAMETERS');
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, n_version, v_sql,sqlcode,sqlerrm);
  END GetGTMVersion;

-- -----------------------------------------------------------------------
-- Revision Set Operations:
-- -----------------------------------------------------------------------

-- Set Active Revision Set to LIVE.
-- Syntax: EXEC GTMUTIL.SetLIVE;
  PROCEDURE SetLive IS
  BEGIN
    GTM.SetActiveRevisionSet('LIVE');
  END SetLive;
--
-- Set Active Revision Set to Parent of specified child.
-- Syntax: EXEC GTMUTIL.SetParent(v_childrevset);
  PROCEDURE SetParent( v_childrev IN VARCHAR2) IS
  c_cmdname         VARCHAR2(12):='SetParent';
  v_parent          VARCHAR2(30);
  BEGIN
    v_parent := GetParentRevSet(v_childrev);
    GTM.SetActiveRevisionSet(v_parent);
  END SetParent;
--
-- Output the name of the active revision set.
-- Syntax: EXEC GTMUTIL.GetActive;
  PROCEDURE GetActive IS
  c_cmdname         VARCHAR2(12):='GetActive';
  v_active          VARCHAR2(30);
  BEGIN
    v_active := GTM.GetActiveRevisionSet;
    goom.Response('Active Revision Set',NVL(v_active,'LIVE'));
  END GetActive;
--
-- List Revision Sets in current schema
-- Syntax: EXEC GTMUTIL.ListRevSets;
  PROCEDURE ListRevSets( v_schema IN VARCHAR2 DEFAULT USER) IS
    v_revlist  GetRevSets%ROWTYPE;
  BEGIN
    GOOM.TitleBlock(v_schema||' - Current Revision Sets', 80); 
    GOOM.DBMSG(RPAD('Revision Set',32)||RPAD('Parent',32)||RPAD('Created',15));
    GOOM.DashLine;
    FOR v_revlist in GetRevSets(v_schema) LOOP
     IF GetRevSets%ROWCOUNT=0 OR GetRevSets%NOTFOUND THEN
       GOOM.DBMSG(RPAD('None',32)||RPAD('None',32)||RPAD('None',15));
       EXIT;
     END IF;
     GOOM.DBMSG(RPAD( v_revlist.WORKSPACE,32)||RPAD( v_revlist.PARENT_WORKSPACE,32)||RPAD( v_revlist.CREATETIME,15)); 
    END LOOP;
    GOOM.DashLine; 
  END ListRevSets;
--
-- List All Revision Sets in instance
-- Syntax: EXEC GTMUTIL.ListAllRevSets;
  PROCEDURE ListAllRevSets IS
    v_revlist  GetAllRevSets%ROWTYPE;
  BEGIN
    GOOM.TitleBlock('All Revision Sets', 105); 
    GOOM.DBMSG(RPAD('Owner',32)||RPAD('Revision Set',32)||RPAD('Parent',32)||RPAD('Created',15));
    GOOM.DashLine(105);
    FOR v_revlist in GetAllRevSets LOOP
     IF GetAllRevSets%ROWCOUNT=0 OR GetAllRevSets%NOTFOUND THEN
     GOOM.DBMSG(RPAD('None',32)||RPAD('None',32)||RPAD('None',32)||RPAD('None',15));
       EXIT;
     END IF;
     GOOM.DBMSG(RPAD( v_revlist.OWNER,32)||RPAD( v_revlist.WORKSPACE,32)||RPAD( v_revlist.PARENT_WORKSPACE,32)||RPAD( v_revlist.CREATETIME,15)); 
    END LOOP;
    GOOM.DashLine(105); 
  END ListAllRevSets;
--
-- List Child Revsets for specified parent.  Default is LIVE.
-- Syntax: EXEC GTMUTIL.ListChildRevSets(v_revsetname); 
  PROCEDURE ListChildRevSets(v_revsetname IN VARCHAR2 DEFAULT 'LIVE') IS
    c_cmdname   CONSTANT VARCHAR2(16) := 'ListChildRevSets';
    --
    v_revlist  GetRevSets%ROWTYPE;
  BEGIN
    GOOM.TitleBlock('Current Child Revision Sets for '||v_revsetname, 80); 
    GOOM.DBMSG(RPAD('Revision Set',32)||RPAD('Owner',32)||RPAD('Created',15));
    GOOM.DashLine;
    FOR v_revlist in GetChildRevSets(v_revsetname) LOOP
     IF GetChildRevSets%ROWCOUNT=0 OR GetChildRevSets%NOTFOUND THEN
       GOOM.DBMSG(RPAD('None',32)||RPAD('None',32)||RPAD('None',15));
       EXIT;
     END IF;
     GOOM.DBMSG(RPAD( v_revlist.WORKSPACE,32)||RPAD( v_revlist.OWNER,32)||RPAD( v_revlist.CREATETIME,15)); 
    END LOOP;
  END ListChildRevSets;
-- 
-- List all Secured Tables in Current Schema.
-- Syntax: EXEC GTMUTIL.ListSecuredTables(v_schema);
  PROCEDURE ListSecuredTables( v_schema IN VARCHAR2 DEFAULT USER) IS
  v_tablist  GetSecuredTableType%ROWTYPE;
  BEGIN
    GOOM.TitleBlock('Secured Tables in: '||v_schema,80);
    GOOM.DBMSG(RPAD('Secured Tables',32)||RPAD('Conflicts?',12)||RPAD('State',40));
    GOOM.DashLine;
    FOR v_revlist in GetSecuredTableType(v_schema) LOOP
     IF GetSecuredTableType%NOTFOUND THEN
     GOOM.DBMSG(RPAD('None',32)||RPAD('None',12)||RPAD('None',30));
       EXIT;
     END IF;
     GOOM.DBMSG(RPAD( v_revlist.TABLE_NAME,32)||RPAD( v_revlist.CONFLICT,12)|| v_revlist.STATE); 
    END LOOP;
    GOOM.DashLine;
  END ListSecuredTables;
--
-- Force a Revision Set removal. Child RevSets will also be removed, access via a DBA only.
-- Syntax: EXEC GTMUTIL.DBADeleteRevSet(v_revsetname, v_schema);
  PROCEDURE DBADeleteRevSet( v_revsetname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER) IS
  c_cmdname  VARCHAR2(19):='DeleteRevisionSet';
  b_success  BOOLEAN     := TRUE;
  v_count    PLS_INTEGER :=0;
  c_sql      VARCHAR2(512);
  BEGIN
     IF GetGTMVersion > 5.2 THEN
       BEGIN
         DBMS_WM.RemoveWorkspaceTree( v_revsetname );
         GOOM.Response( c_cmdname,'Workspace Tree: '|| v_revsetname ||' deleted from Workspace Manager.');
       EXCEPTION
       WHEN OTHERS THEN
         b_success := FALSE;
         GOOM.REPORT_ERROR ( c_cmdname, v_revsetname,'DBMS_WM.RemoveWorkspaceTree', sqlcode, sqlerrm);
       END;
     ELSE
       BEGIN
         DBMS_WM.RemoveWorkspace( v_revsetname );
         GOOM.Response( c_cmdname,'Workspace: '|| v_revsetname ||' deleted from Workspace Manager.');
       EXCEPTION
       WHEN OTHERS THEN
         b_success := FALSE;
         GOOM.REPORT_ERROR ( c_cmdname, v_revsetname,'DBMS_WM.RemoveWorkspace', sqlcode, sqlerrm);
       END;
     END IF;
     IF b_success THEN
        c_sql := 'SELECT COUNT(*) FROM GDOSYS.LTT_REVISION_SETS_BASE WHERE REVSET_NAME = :revset AND SCHEMA_NAME=:vowner';
        EXECUTE IMMEDIATE c_sql INTO v_count USING v_revsetname, v_schema;
        IF v_count <> 0 THEN
          BEGIN
            c_sql := 'DELETE FROM GDOSYS.LTT_REVISION_SETS_BASE WHERE REVSET_NAME = :revset AND SCHEMA_NAME=:vowner';
            EXECUTE IMMEDIATE c_sql using v_revsetname, v_schema;
            COMMIT;
            GOOM.Response( c_cmdname,'Revision Set: '|| v_revsetname ||' deleted from LTT_REVISION_SETS_BASE.');
          EXCEPTION
            WHEN OTHERS THEN
            GOOM.Response( c_cmdname,'No write access to LTT_REVISION_SETS_BASE, you need to be a DBA to run this command');
          END;
        ELSE
          GOOM.Response( c_cmdname,'Revision Set: '|| v_revsetname ||' does not exist in LTT_REVISION_SETS_BASE.');
        END IF;
        COMMIT;
     END IF;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,v_revsetname,c_sql,sqlcode,sqlerrm);
  END DBADeleteRevSet;
--
-- Run as DBA to force the removal of all revision sets for specific user or system wide if v_schema is NULL.
-- Syntax: EXEC GTMUTIL.DBADeleteAllRevSets(v_schema);
  PROCEDURE DBADeleteAllRevSets( v_schema IN VARCHAR2 DEFAULT NULL) IS
    c_cmdname       CONSTANT VARCHAR2(19) := 'DBADeleteAllRevSets';
    c_feedback      CONSTANT VARCHAR2(40) := 'Revision Sets Retired/Deleted: ';
    --
    v_revlist       GetRevSets%ROWTYPE;  
    v_count         PLS_INTEGER:=0;
  BEGIN
    GOOM.Response(c_cmdname,c_msgStart||v_schema,28);
    GOOM.DotLine;
    SetLive;
    IF v_schema IS NULL THEN
      FOR v_revlist in GetAllTopRevSets LOOP
       IF GetAllTopRevSets%ROWCOUNT=0 OR GetAllTopRevSets%NOTFOUND THEN
         EXIT;
       END IF;
        DBADeleteRevSet( v_revlist.WORKSPACE, v_revlist.OWNER);
        v_count:=v_count+1;
      END LOOP;
    ELSE
      FOR v_revlist in GetTopRevSets(v_schema) LOOP
        IF GetTopRevSets%ROWCOUNT=0 OR GetTopRevSets%NOTFOUND THEN
          EXIT;
        END IF;
        DBADeleteRevSet( v_revlist.WORKSPACE, v_schema);
        v_count:=v_count+1;
      END LOOP;
    END IF;
    GOOM.Response( c_cmdname, c_feedback|| v_count);
    GOOM.DotLine;
    GOOM.Response( c_cmdname, c_msgComplete|| v_schema,28);
    GOOM.DblLine;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_revlist.WORKSPACE, v_count,sqlcode,sqlerrm);
  END DBADeleteAllRevSets;
--
-- Retire all Revsets in user
-- Syntax: EXEC GTMUTIL.RetireAllRevSets
  PROCEDURE RetireAllRevSets IS
    c_cmdname       CONSTANT VARCHAR2(19) := 'RetireAllRevSets';
    c_feedback      CONSTANT VARCHAR2(40) := 'Revision Sets Retired/Deleted: ';
    --
    v_revlist       GetRevSets%ROWTYPE;  
    v_count         PLS_INTEGER := 0;
  BEGIN
    GOOM.Response(c_cmdname,c_msgStart||USER,28);
    GOOM.DotLine;
    SetLive;
    FOR v_revlist in GetTopRevSets(USER) LOOP
      IF GetTopRevSets%ROWCOUNT=0 OR GetTopRevSets%NOTFOUND THEN
        EXIT;
      END IF;
      IF GetGTMVersion > 5.2 THEN
        BEGIN
          UPDATE GDOSYS.LTT_REVISION_SETS
             SET REVSET_STATUS = 'R', RETIRE_DATE = SYSDATE
           WHERE REVSET_STATUS = 'O'
             AND REVSET_NAME = v_revlist.WORKSPACE;
           COMMIT;
           DBMS_WM.RemoveWorkspaceTree( v_revlist.WORKSPACE );
           GOOM.Response( c_cmdname,'Workspace Tree: '|| v_revlist.WORKSPACE ||' deleted from Workspace Manager.');
        EXCEPTION
        WHEN OTHERS THEN
           GOOM.REPORT_ERROR ( c_cmdname, v_revlist.WORKSPACE,'DBMS_WM.RemoveWorkspaceTree', sqlcode, sqlerrm);
           ROLLBACK;
        END;
      ELSE
        BEGIN
          UPDATE GDOSYS.LTT_REVISION_SETS
             SET REVSET_STATUS = 'R', RETIRE_DATE = SYSDATE
           WHERE REVSET_STATUS = 'O'
             AND REVSET_NAME = v_revlist.WORKSPACE;
           COMMIT;
           DBMS_WM.RemoveWorkspace( v_revlist.WORKSPACE );
           GOOM.Response( c_cmdname,'Workspace: '|| v_revlist.WORKSPACE ||' deleted from Workspace Manager.');
        EXCEPTION
        WHEN OTHERS THEN
           GOOM.REPORT_ERROR ( c_cmdname, v_revlist.WORKSPACE,'DBMS_WM.RemoveWorkspace', sqlcode, sqlerrm);
        END;
      END IF;
      v_count:=v_count+1;
    END LOOP;
    GOOM.Response( c_cmdname, c_feedback|| v_count);
    GOOM.DotLine;
    GOOM.Response( c_cmdname, c_msgComplete|| USER,28);
    GOOM.DblLine;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_revlist.WORKSPACE, v_count,sqlcode,sqlerrm);
  END RetireAllRevSets;
--
-- Automatically Create Revision Set Name using a unique random string.
-- Syntax: v_revsetname:= GTMUTIL.GenerateRevSetName;
  FUNCTION GenerateRevSetName RETURN VARCHAR2 IS
    v_prefix	    VARCHAR2(5):='Rev:';
    v_suffix	    VARCHAR2(20);
    v_revsetname 	VARCHAR2(30);
  BEGIN
    v_suffix:=SUBSTR(SYS_GUID(),-20);
    v_revsetname:=v_prefix||v_suffix;
    RETURN v_revsetname;
  END GenerateRevSetName;
--
-- Automatically Create Multiple Revision Sets.  Pass number required and TRUE if optimistic.
-- Syntax: EXEC GTMUTIL.CreateMultiRevSets(i_num, b_opt);
  PROCEDURE CreateMultiRevSets( i_num IN INTEGER, b_opt IN BOOLEAN DEFAULT FALSE) IS
    c_cmdname           CONSTANT VARCHAR2(18) := 'CreateMultiRevSets';
    c_feedback          CONSTANT VARCHAR2(41) := 'Number of Revision Sets Created: ';
    --
    v_revsetname        VARCHAR2(30);
    v_desc              VARCHAR2(30);
    v_user              VARCHAR2(64);
    I                   PLS_INTEGER;
  BEGIN
    v_desc       := 'Auto Generated - Rev Set:';
    v_user       := SYS_CONTEXT('USERENV','CLIENT_IDENTIFIER');
    FOR I IN 1..i_num LOOP
      v_revsetname := GenerateRevSetName;
      GTM.CreateRevisionSet( v_revsetname, v_desc||I, v_user, TRUE, b_opt);
    END LOOP;
    GOOM.Response( c_cmdname, c_feedback|| i_num);
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR( c_cmdname, v_revsetname, v_desc||I, sqlcode, sqlerrm);
  END CreateMultiRevSets;
--
-- Return the parent of the specified workspace.
-- Syntax: v_parentRevSet:= GTMUTIL.GetParentRevSet(v_revsetname, v_schema);
  FUNCTION GetParentRevSet( v_revsetname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER) RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(15) :='GetParentRevSet';
  --
    v_sql           VARCHAR2(255);
    v_parent        VARCHAR2(30);
  BEGIN
    IF GetGTMVersion > 5.2 THEN
      v_sql := 'SELECT PARENT_WORKSPACE FROM ALL_WORKSPACES WHERE OWNER = :vowner AND WORKSPACE = :revset';
      EXECUTE IMMEDIATE v_sql INTO v_parent USING v_schema, v_revsetname;
      RETURN v_parent;
    ELSE
      RETURN 'LIVE';
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_revsetname, v_sql,sqlcode,sqlerrm);
      RETURN NULL;
  END GetParentRevSet;

-- -----------------------------------------------------------------------
-- Spatial Index and Tuning Operations
-- -----------------------------------------------------------------------

-- Spatially index a version enabled table.
-- Syntax: EXEC GTMUTIL.GTMSpatialIndex(v_table, b_opt); 
--         b_opt = FALSE will not contstrain the geometry to a type, default is TRUE.
  PROCEDURE GTMSpatialIndex( v_tablename IN VARCHAR2, b_opt IN BOOLEAN DEFAULT TRUE) IS
    c_cmdname         CONSTANT VARCHAR2(19) := 'GTMSpatialIndex';
    c_cmdtype         CONSTANT VARCHAR2(6)  := 'TUNING';
    c_feedback1       CONSTANT VARCHAR2(30) := 'Beginning DDL Operations for: ';
    c_feedback2       CONSTANT VARCHAR2(30) := 'Commiting DDL Operations for: ';
    c_ext             CONSTANT VARCHAR2(4)  := '_LTS';
      -- added for owner.table support
    v_ownertable      VARCHAR2(61);
    BEGIN
      v_ownertable := GOOM.GetOwnerObject(v_tablename);
      --
      GOOM.Response(c_cmdname, c_feedback1||v_ownertable);
      DBMS_WM.BeginDDL(v_ownertable);
      GOOM.SpatialIndex(v_ownertable||c_ext, b_opt);
      DBMS_WM.CommitDDL(v_ownertable);
      GOOM.Response(c_cmdname, c_feedback2||v_ownertable);
    EXCEPTION
      WHEN OTHERS THEN
        DBMS_WM.RollbackDDL(v_tablename);
        GOOM.REPORT_ERROR(c_cmdname, v_ownertable, c_msgIndexingFailed || v_ownertable, SQLCODE, SQLERRM);
  END GTMSpatialIndex;
-- 
-- Spatially index all version enabled tables in schema.
-- Syntax: EXEC GTMUTIL.GTMSpatialIndexAll(v_schema, b_opt);
--         b_opt = FALSE will not contstrain the geometry to a type, default is TRUE.
  PROCEDURE GTMSpatialIndexAll( v_schema IN VARCHAR2 DEFAULT USER, b_opt IN BOOLEAN DEFAULT TRUE) IS
    c_cmdname         CONSTANT VARCHAR2(19) := 'GTMSpatialIndexAll';
    c_cmdtype         CONSTANT VARCHAR2(6)  := 'TUNING';
    c_appliesto       CONSTANT VARCHAR2(30) := 'All Secured Tables';
    --
    v_feature     GetSecureTableName%ROWTYPE;
    v_ownertable  VARCHAR2(61);
    --
    BEGIN
      GOOM.DotLine;
      GOOM.Response(c_cmdname, c_msgStart || v_schema,28);
      FOR v_feature IN GetSecureTableName(v_schema) LOOP
        v_ownertable  := v_schema||'.'||v_feature.LTT_VIEW_NAME;
        GTMSpatialIndex( v_ownertable, b_opt);
      END LOOP;
      GOOM.DotLine;
      GOOM.Response( c_cmdname, c_msgComplete || v_schema,28);
      GOOM.DashLine;
    EXCEPTION
      WHEN OTHERS THEN
        GOOM.REPORT_ERROR(c_cmdname, v_ownertable,c_msgIndexingFailed || v_ownertable, SQLCODE, SQLERRM);
    END GTMSpatialIndexAll;
-- 
-- Gather table level statistics on all version enabled tables.
-- Syntax: EXEC GTMUTIL.AnalyzeLT_TABLES(v_schema);
  PROCEDURE AnalyzeLT_TABLES( v_schema IN VARCHAR2 DEFAULT USER) IS
    CURSOR GetTables( v_schema VARCHAR2 DEFAULT USER) IS 
           SELECT TABLE_NAME FROM ALL_TABLES WHERE TABLE_NAME LIKE '%_LT' AND OWNER LIKE v_schema;
    c_cmdname       CONSTANT VARCHAR2(16) := 'AnalyzeLT_TABLES';
    c_feedback      CONSTANT VARCHAR2(30) := 'DB Statistics updated for: ';
    --
    v_table         GetTables%ROWTYPE;
  BEGIN
    FOR v_table IN GetTables(v_schema) LOOP
  	DBMS_STATS.GATHER_TABLE_STATS(ownname=> v_schema,
  	                              tabname=> v_table.table_name,
  	                          granularity=> 'DEFAULT',
  	                         block_sample=> FALSE,
  	                              cascade=> TRUE,
  	                               degree=> 1,
  	                           method_opt=> 'FOR ALL COLUMNS SIZE AUTO');
    END LOOP;
    Goom.Response(c_cmdname, c_feedback||v_schema);
    EXCEPTION
      WHEN OTHERS THEN
        GOOM.REPORT_ERROR(c_cmdname, v_schema, v_table.table_name, SQLCODE, SQLERRM);
  END AnalyzeLT_TABLES; 

-- -----------------------------------------------------------------------
-- Privilege Operations
-- -----------------------------------------------------------------------

-- Grant DML privilege on all version enabled tables in schema
-- Syntax: EXEC GTMUTIL.GrantPrivOnSchema('<grantee>','<access>','<ddl>');
--         grantee the user getting the privilege
--         access 'R' for read only and 'W' for read-write
--         ddl  'N' to run, 'Y' to generate statements.
  PROCEDURE GrantPrivOnSchema(v_grantee IN VARCHAR2, v_access IN VARCHAR2 DEFAULT 'R', v_ddl IN VARCHAR2 DEFAULT 'N') IS
  CURSOR GetGTMObjs is   SELECT object_name ,object_type
                           FROM all_objects 
                          WHERE object_type IN ('TABLE','VIEW')
                            AND OBJECT_NAME NOT IN ('XDB')
                            AND OBJECT_NAME NOT LIKE 'BIN$%'						  
  				          AND OWNER = USER;			
  -- This cursor gets all GTM Objects that are writable.  Used to grant other privs.
  Cursor GetGTMRWObjs is SELECT object_name ,object_type
                           FROM all_objects 
                          WHERE object_type IN ('TABLE','VIEW')
                            AND OBJECT_NAME NOT IN ('XDB')
                            AND OBJECT_NAME NOT LIKE 'BIN$%'
                            AND OBJECT_NAME NOT LIKE '%$'
  				          AND OBJECT_NAME NOT LIKE '%_CONS'
  					      AND OBJECT_NAME NOT LIKE '%_AUX'
  					      AND OBJECT_NAME NOT LIKE '%_PKC'
  					      AND OBJECT_NAME NOT LIKE '%_PKD'
  					      AND OBJECT_NAME NOT LIKE '%_PKDB'
  					      AND OBJECT_NAME NOT LIKE '%_PKDC'
  					      AND OBJECT_NAME NOT LIKE '%_LOCK'
                            AND OBJECT_NAME NOT LIKE '%_MODS'
                            AND OBJECT_NAME NOT LIKE '%_DIFF'		
                            AND OBJECT_NAME NOT LIKE '%_CONF'	
                            AND OBJECT_NAME NOT LIKE '%_HIST'
                            AND OBJECT_NAME NOT LIKE '%_MW'	
                            AND OBJECT_NAME NOT LIKE '%_BASE'		  
  					      AND OWNER = USER;			
  -- This cursor gets the sequences for either select or select and alter.				  		   						  
  Cursor GetGDOseq is SELECT object_name 
                        FROM all_objects 
                       WHERE object_type ='SEQUENCE'
  				       AND OBJECT_NAME NOT IN ('XDB')
                         AND OBJECT_NAME NOT LIKE 'BIN$%'
  				       AND OBJECT_NAME NOT LIKE 'WMSYS%'
  				       AND OWNER = USER;
  -- Variable assignements.						  
  c_cmdname      VARCHAR2(32):='GrantPrivOnSchema';
  v_GTMtables    GetGTMObjs%ROWTYPE;
  v_RWtables     GetGTMRWObjs%ROWTYPE;
  v_seq          GetGDOSeq%ROWTYPE;
  v_objperm      VARCHAR2(255);
  v_seqperm      VARCHAR2(255);
  v_sql          VARCHAR2(512);
  v_lttexist     PLS_INTEGER;
  v_msg          VARCHAR2(255);
  v_debug        VARCHAR2(255);
  BEGIN
     CASE v_access
       WHEN 'W' THEN
         v_objperm:='ALL';
         v_seqperm:='SELECT, ALTER';
         v_msg:='Full Read-Write Access on '||USER||' granted to '||v_grantee;
       WHEN 'R' THEN
         v_objperm:='SELECT';
         v_seqperm:='SELECT';
       ELSE 
         GOTO exit_early;
     END CASE;
     -- Grant select on everything for read only access
     FOR v_GTMtables in GetGTMObjs LOOP
       v_sql:='GRANT SELECT on '||USER||'.'||v_GTMtables.object_name||' TO '||v_grantee;
       IF v_ddl = 'Y' THEN
         GOOM.DBMSG(v_sql||';');
       ELSE
         EXECUTE IMMEDIATE v_sql;
       END IF;
     END LOOP;
     -- Grant all on objects for RW access.
     IF v_access = 'W' THEN
       FOR v_RWtables in GetGTMRWObjs LOOP
         v_sql:='GRANT '||v_objperm||' ON '||USER||'.'||v_RWtables.object_name||' TO '||v_grantee;
         IF v_ddl = 'Y' THEN
           GOOM.DBMSG(v_sql||';');
         ELSE
           EXECUTE IMMEDIATE (v_sql);
         END IF;
       END LOOP;
     -- Grants for sequences dependng on access type.
       FOR v_seq in GetGDOseq LOOP
         v_sql:='GRANT '||v_seqperm||' on '||USER||'.'||v_seq.object_name||' TO '||v_grantee;
         IF v_ddl ='Y' THEN
           GOOM.DBMSG(v_sql||';');
         ELSE
           EXECUTE IMMEDIATE v_sql;
         END IF;
       END LOOP;
     END IF;
     <<exit_early>>
     NULL;
  EXCEPTION
     WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_sql,'None',sqlcode,sqlerrm);
  END GrantPrivOnSchema;

-- Grant DML privilege on version enabled table
-- Syntax: EXEC GTMUTIL.GrantPrivOnTable('<grantee>','<fc>','<access>','<ddl>');
--         grantee - the user getting the privilege
--         fc      - the feature class name
--         access  - 'R' for read only and 'W' for read-write
--         ddl     - 'N' to run, 'Y' to generate statements.
  PROCEDURE GrantPrivOnTable(v_grantee IN VARCHAR2, v_fc IN VARCHAR2, v_access IN VARCHAR2 DEFAULT 'R', v_ddl IN VARCHAR2 DEFAULT 'N') IS
  CURSOR GetGTMObjs is   SELECT object_name ,object_type
                           FROM all_objects 
                          WHERE object_type IN ('TABLE','VIEW')
                            AND OBJECT_NAME NOT IN ('XDB')
                            AND OBJECT_NAME NOT LIKE 'BIN$%'						  
  				          AND OWNER = USER
                            AND OBJECT_NAME LIKE UPPER(v_fc||'%');			
  -- This cursor gets all GTM Objects that are writable.  Used to grant other privs.
  Cursor GetGTMRWObjs is SELECT object_name 
                           FROM all_objects 
                          WHERE OBJECT_NAME LIKE UPPER(v_fc||'_LT') 
                             OR OBJECT_NAME LIKE UPPER(v_fc);		
  -- This cursor gets the sequences for either select or select and alter.					  		   						  
  Cursor GetGDOseq is SELECT object_name 
                        FROM all_objects 
                       WHERE object_type ='SEQUENCE'
  				       AND OBJECT_NAME NOT IN ('XDB')
                         AND OBJECT_NAME NOT LIKE 'BIN$%'
  				       AND OBJECT_NAME NOT LIKE 'WMSYS%'
  				       AND OWNER = USER
                         AND OBJECT_NAME LIKE UPPER(v_fc||'%');
  -- Variable assignements.						  
  c_cmdname      VARCHAR2(32):='GrantPrivOnTable';
  v_GTMtables    GetGTMObjs%ROWTYPE;
  v_RWtables     GetGTMRWObjs%ROWTYPE;
  v_seq          GetGDOSeq%ROWTYPE;
  v_objperm      VARCHAR2(255);
  v_seqperm      VARCHAR2(255);
  v_sql          VARCHAR2(512);
  v_lttexist     PLS_INTEGER;
  v_msg          VARCHAR2(255);
  v_debug        VARCHAR2(255);
  BEGIN
     CASE v_access
       WHEN 'W' THEN
         v_objperm:='ALL';
         v_seqperm:='SELECT, ALTER';
         v_msg:='Full Read-Write Access on '||USER||' granted to '||v_grantee;
       WHEN 'R' THEN
         v_objperm:='SELECT';
         v_seqperm:='SELECT';
       ELSE 
         GOTO exit_early;
     END CASE;
     -- Grant select on everything for read only access
       FOR v_GTMtables in GetGTMObjs LOOP
         v_sql:='GRANT SELECT on '||USER||'.'||v_GTMtables.object_name||' TO '||v_grantee;
         IF v_ddl = 'Y' THEN
           GOOM.DBMSG(v_sql||';');
         ELSE
           EXECUTE IMMEDIATE v_sql;
         END IF;
       END LOOP;
     -- Grant all on objects for RW access.
     IF v_access = 'W' THEN
       FOR v_RWtables in GetGTMRWObjs LOOP
         v_sql:='GRANT '||v_objperm||' ON '||USER||'.'||v_RWtables.object_name||' TO '||v_grantee;
         IF v_ddl = 'Y' THEN
           GOOM.DBMSG(v_sql||';');
         ELSE
           EXECUTE IMMEDIATE (v_sql);
         END IF;
       END LOOP;
     -- Grants for sequences dependng on access type.
       FOR v_seq in GetGDOseq LOOP
         v_sql:='GRANT '||v_seqperm||' on '||USER||'.'||v_seq.object_name||' TO '||v_grantee;
         IF v_ddl ='Y' THEN
           GOOM.DBMSG(v_sql||';');
         ELSE
           EXECUTE IMMEDIATE v_sql;
         END IF;
       END LOOP;
     END IF;
     <<exit_early>>
     NULL;
  EXCEPTION
     WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_sql,'None',sqlcode,sqlerrm);
  END GrantPrivOnTable;

-- -----------------------------------------------------------------------
-- Schema Operations
-- -----------------------------------------------------------------------

-- Disable All Triggers in a schema
-- Syntax: EXEC GTMUTIL.DisableAllTriggers(v_schema);
  PROCEDURE DisableAllTriggers( v_schema IN VARCHAR2 DEFAULT USER)  IS
    --
    c_cmdname     CONSTANT VARCHAR2(18) := 'DisableAllTriggers';
    c_feedback    CONSTANT VARCHAR2(18) := 'Trigger Disabled: ';
    --
    v_trig    GetTriggerName%ROWTYPE;
    v_trigger VARCHAR2(30);
    v_table   VARCHAR2(30);
  BEGIN
    FOR v_trig in GetTriggerName(v_schema) LOOP
        v_trigger := v_trig.TRIGGER_NAME;
        v_table   := v_trig.TABLE_NAME;
        IF GTM.IsTableSecure(v_schema||'.'||v_table) = 'YES' THEN
          DBMS_WM.BeginDDL(v_schema||'.'||v_table);
          GOOM.RESPONSE(c_cmdname,c_feedback||v_schema||'.'||v_trigger);
          EXECUTE IMMEDIATE 'ALTER TRIGGER '||v_schema||'.'||v_trigger||' DISABLE';
          DBMS_WM.CommitDDL(v_schema||'.'||v_table);
        ELSE
          GOOM.RESPONSE(c_cmdname,c_feedback||v_schema||'.'||v_trigger);
          EXECUTE IMMEDIATE 'ALTER TRIGGER '||v_schema||'.'||v_trigger||' DISABLE';
        END IF;
    END LOOP;
  EXCEPTION
     WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_table||'.'||v_trigger,'NONE',sqlcode,sqlerrm);
  END DisableAllTriggers;
--
-- Enable All Triggers in a schema
-- Syntax: EXEC GTMUTIL.EnableAllTriggers(v_schema);
  PROCEDURE EnableAllTriggers( v_schema IN VARCHAR2 DEFAULT USER) IS
    c_cmdname     CONSTANT VARCHAR2(18) := 'EnableAllTriggers';
    c_feedback    CONSTANT VARCHAR2(18) := 'Trigger Enabled: ';
    --
    v_trig    GetTriggerName%ROWTYPE;
    v_trigger VARCHAR2(30);
    v_table   VARCHAR2(30);
  BEGIN
    EXECUTE IMMEDIATE 'PURGE RECYCLEBIN';
    FOR v_trig in GetTriggerName(v_schema) LOOP
        v_trigger:=v_trig.TRIGGER_NAME;
        v_table:=v_trig.TABLE_NAME;
        IF GTM.IsTableSecure(v_schema||'.'||v_table) = 'YES' THEN
          DBMS_WM.BeginDDL(v_schema||'.'||v_table);
          GOOM.RESPONSE(c_cmdname,c_feedback||v_schema||'.'||v_trigger);
          EXECUTE IMMEDIATE 'ALTER TRIGGER '||v_trigger||' ENABLE';
          DBMS_WM.CommitDDL(v_schema||'.'||v_table);
        ELSE
          GOOM.RESPONSE(c_cmdname,c_feedback||v_schema||'.'||v_trigger);
          EXECUTE IMMEDIATE 'ALTER TRIGGER '||v_schema||'.'||v_trigger||' ENABLE';
          EXECUTE IMMEDIATE 'ALTER TRIGGER '||v_schema||'.'||v_trigger||' COMPILE';
        END IF;
    END LOOP;
  EXCEPTION
     WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_table||'.'||v_trigger,'NONE',sqlcode,sqlerrm);
  END EnableAllTriggers;
--
-- Verify Schema conforms to rules for both GTM and Workspace Manager.
-- Syntax: EXEC GTMUTIL.VerifySchema( v_schema);
  PROCEDURE VerifySchema(v_schema IN VARCHAR2 DEFAULT USER) IS
    CURSOR ChkTableLength(v_user VARCHAR2 DEFAULT USER)  IS 
           SELECT TABLE_NAME FROM ALL_TABLES WHERE OWNER = v_user AND LENGTH(TABLE_NAME)>22;
    CURSOR ChkColumnLength(v_user VARCHAR2 DEFAULT USER) IS 
           SELECT TABLE_NAME,COLUMN_NAME FROM ALL_TAB_COLS WHERE OWNER = v_user AND LENGTH(COLUMN_NAME)>28;
    CURSOR ChkIndexLength(v_user VARCHAR2 DEFAULT USER)  IS 
           SELECT TABLE_NAME,INDEX_NAME FROM ALL_INDEXES WHERE OWNER = v_user AND LENGTH(INDEX_NAME)>26;
    CURSOR ChkConLength(v_user VARCHAR2 DEFAULT USER)    IS 
           SELECT TABLE_NAME,CONSTRAINT_NAME FROM ALL_CONSTRAINTS WHERE OWNER = v_user AND LENGTH(CONSTRAINT_NAME)>27;
    CURSOR ChkTrigLength(v_user VARCHAR2 DEFAULT USER)   IS 
           SELECT TRIGGER_NAME FROM ALL_TRIGGERS WHERE OWNER = v_user AND LENGTH(TRIGGER_NAME)>27;
    CURSOR ChkTrigEvent(v_user VARCHAR2 DEFAULT USER)    IS 
           SELECT TRIGGER_NAME,TABLE_NAME FROM ALL_TRIGGERS WHERE OWNER = v_user AND TRIGGERING_EVENT NOT IN ('INSERT','UPDATE','DELETE');
    CURSOR ChkResvCols(v_user VARCHAR2 DEFAULT USER)     IS 
           SELECT TABLE_NAME, COLUMN_NAME FROM ALL_TAB_COLS WHERE OWNER = v_user AND COLUMN_NAME IN ('DELSTATUS','VERSION','NEXTVER','LTLOCK','CREATETIME','RETIRETIME');
    CURSOR ChkResvTabs(v_user VARCHAR2 DEFAULT USER)     IS 
           SELECT TABLE_NAME FROM ALL_TABLES WHERE OWNER = v_user AND TABLE_NAME LIKE '%\_G' ESCAPE '\';
    --
    c_cmdname     CONSTANT VARCHAR2(12) :='VerifySchema';
    c_indicator   CONSTANT VARCHAR2(3)  := '-> ';
    --
    v_test        VARCHAR2(64);
    v_count       INTEGER;
  BEGIN
    EXECUTE IMMEDIATE 'PURGE RECYCLEBIN';
    GOOM.DashLine;
    GOOM.TitleLine('GTM Schema Verification for: '||v_schema,80,'---');
    GOOM.TitleLine('The schema must pass the following tests before tables can be secured.',80,'-');
    GOOM.DotLine;
    v_test:='TABLE NAME LENGTH <=22 (use to be 25)';
    SELECT count(1) INTO v_count FROM ALL_TABLES WHERE OWNER = v_schema AND LENGTH(TABLE_NAME)>25;
    IF v_count>0 THEN
      GOOM.RESPONSE(v_test,'Failed: These table names are too long:',40); 
      FOR v_tab IN ChkTableLength(v_schema) LOOP
        GOOM.DBMSG(c_indicator||v_tab.TABLE_NAME); 
      END LOOP; 
      GOOM.DotLine;
    ELSE
       GOOM.RESPONSE(v_test,'Passed: All tables names are compliant.',40);
    END IF;
    v_test:='TABLE NAME ENDS WITH _G';
    SELECT count(1) INTO v_count FROM ALL_TABLES WHERE OWNER = v_schema AND TABLE_NAME LIKE '%\_G' ESCAPE '\';
    IF v_count>0 THEN
      GOOM.RESPONSE(v_test,'Failed: These table names end with _G:',40); 
      FOR v_tab IN ChkResvTabs(v_schema)  LOOP
        GOOM.DBMSG(c_indicator||v_schema||'.'||v_tab.TABLE_NAME);  
      END LOOP; 
      GOOM.DotLine;
    ELSE
       GOOM.RESPONSE(v_test,'Passed: All tables names are compliant.',40);
    END IF;
    v_test:='COLUMN NAME LENGTH <=28';
    SELECT count(1) INTO v_count FROM ALL_TAB_COLS WHERE OWNER = v_schema AND LENGTH(COLUMN_NAME)>28;
    IF v_count>0 THEN
      GOOM.RESPONSE(v_test,'Failed: These column names are too long:',40); 
      FOR v_tab IN ChkColumnLength(v_schema)  LOOP
        GOOM.DBMSG(c_indicator||v_schema||'.'||v_tab.TABLE_NAME||'.'||v_tab.COLUMN_NAME);  
      END LOOP;
      GOOM.DotLine; 
    ELSE
       GOOM.RESPONSE(v_test,'Passed: All columns names are compliant.',40);
    END IF;
    v_test:='COLUMN NAMES RESERVED';
    SELECT COUNT(*) into v_count FROM ALL_TAB_COLS WHERE OWNER = v_schema AND COLUMN_NAME IN ('DELSTATUS','VERSION','NEXTVER','LTLOCK','CREATETIME','RETIRETIME');
    IF v_count>0 THEN
      GOOM.RESPONSE(v_test,'Failed: Reserved column names, fix or delete:',40); 
      FOR v_tab IN ChkResvCols(v_schema)  LOOP
        GOOM.dbmsg(c_indicator||v_schema||'.'||v_tab.TABLE_NAME||'.'||v_tab.COLUMN_NAME);      
      END LOOP;
      GOOM.DotLine; 
    ELSE
       GOOM.RESPONSE(v_test,'Passed: No reserved column names used.',40);
    END IF;
    v_test:='INDEX NAME LENGTH <=26';
    SELECT count(1) INTO v_count FROM ALL_INDEXES WHERE OWNER = v_schema AND LENGTH(INDEX_NAME)>26;
    IF v_count>0 THEN
      GOOM.RESPONSE(v_test,'Failed: These index names are too long:',40); 
      FOR v_tab IN ChkIndexLength(v_schema)  LOOP
        GOOM.DBMSG(c_indicator||v_schema||'.'||v_tab.TABLE_NAME||'.'||v_tab.INDEX_NAME); 
      END LOOP; 
      GOOM.DotLine;
    ELSE
       GOOM.RESPONSE(v_test,'Passed: All index names are compliant.',40);
    END IF;
    v_test:='CONSTRAINT NAME LENGTH <=27';
    SELECT count(1) INTO v_count FROM ALL_CONSTRAINTS WHERE OWNER = v_schema AND LENGTH(CONSTRAINT_NAME)>27;
    IF v_count>0 THEN
      GOOM.RESPONSE(v_test,'Failed: These constraint names are too long:'); 
      FOR v_tab IN ChkConLength(v_schema)  LOOP
        GOOM.DBMSG(c_indicator||v_schema||'.'||v_tab.TABLE_NAME||'.'||v_tab.CONSTRAINT_NAME); 
      END LOOP; 
      GOOM.DotLine;
    ELSE
       GOOM.RESPONSE(v_test,'Passed: All constraint names are compliant.',40);
    END IF;
    v_test:='TRIGGER NAME LENGTH <=27';
    SELECT count(1) INTO v_count FROM ALL_TRIGGERS WHERE OWNER = v_schema AND LENGTH(TRIGGER_NAME)>27;
    IF v_count>0 THEN
      GOOM.RESPONSE(v_test,'Failed: These trigger names are too long:',40); 
      FOR v_tab IN ChkTrigLength(v_schema)  LOOP
        GOOM.DBMSG(c_indicator||v_schema||'.'||v_tab.TRIGGER_NAME);     
      END LOOP; 
      GOOM.DotLine;
    ELSE
       GOOM.RESPONSE(v_test,'Passed: All trigger names are compliant.',40);
    END IF;
    v_test:='TRIGGER EVENT';
    SELECT COUNT(*) INTO v_count FROM ALL_TRIGGERS WHERE OWNER = v_schema AND TRIGGERING_EVENT NOT IN ('INSERT','UPDATE','DELETE');
    IF v_count>0 THEN
      GOOM.RESPONSE(v_test,'Failed: Incorrect trigger type, fix or delete.:',40); 
      FOR v_tab IN ChkTrigEvent(v_schema)  LOOP
        GOOM.DBMSG(c_indicator||v_schema||'.'||v_tab.TABLE_NAME||'.'||v_tab.TRIGGER_NAME);     
      END LOOP; 
      GOOM.DotLine;
    ELSE
       GOOM.RESPONSE(v_test,'Passed: All triggering events are compliant.',40);
    END IF;
    GOOM.DashLine;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,'','',sqlcode,sqlerrm);
  END VerifySchema;
--
-- Drop a GTM user containing secured tables and cleanup afterwards.
-- Syntax: EXEC GTMUTIL.DropGTMUser
  PROCEDURE DropGTMUser (v_username in VARCHAR2) is
    c_cmdname   CONSTANT VARCHAR2(12):='DropGTMUser';
    v_sql       VARCHAR2(255);
    v_msg       VARCHAR2(60);
  BEGIN
    GOOM.Response(c_cmdname, c_msgStart||v_username,28);
    DBADeleteAllRevSets(v_username);
    v_sql:='DROP USER '||v_username||' CASCADE';
    EXECUTE IMMEDIATE v_sql;
    v_msg:='USER dropped: '||v_username;
    GOOM.RESPONSE(c_cmdname,v_msg);
    DBMS_WM.RecoverFromDroppedUser;
    v_msg:='DBMS_WM RECOVER: '||v_username;
    GOOM.RESPONSE(c_cmdname,v_msg);
    v_sql:='DELETE FROM GDOSYS.LTT_TABLES WHERE SCHEMA_NAME=:schema';
    EXECUTE IMMEDIATE v_sql USING v_username;
    v_msg:='GDOSYS.LTT_TABLES: Metadata deleted.';
    GOOM.RESPONSE(c_cmdname,v_msg);
    v_sql:='DELETE FROM GDOSYS.LTT_REVISION_SETS_BASE WHERE SCHEMA_NAME=:schema';
    EXECUTE IMMEDIATE v_sql USING v_username;
    v_msg:='GDOSYS.LTT_REVISION_SETS: Metadata deleted.';
    GOOM.RESPONSE(c_cmdname,v_msg);
    COMMIT;
    GOOM.Response(c_cmdname, c_msgComplete||v_username,28);
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,v_sql,v_msg,sqlcode,sqlerrm);
  END DropGTMUser;

-- -----------------------------------------------------------------------
-- Valid Time Operations
-- -----------------------------------------------------------------------

-- These functions return the number of records currently affected by
-- Valid Time enabled DML operations.
--
  FUNCTION GetVTUpdateCount(v_tablename IN VARCHAR2) RETURN INTEGER IS
  c_cmdname VARCHAR2(16):='GetVTUpdateCount';
  v_count   INTEGER;
  v_sql     VARCHAR2(64);
  BEGIN
    v_sql:='SELECT COUNT(1) FROM '||v_tablename||'_HIST WHERE WM_OPTYPE=''U''';
    EXECUTE IMMEDIATE v_sql INTO v_count;
    RETURN v_count;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,v_sql,v_tablename,sqlcode,sqlerrm);
  END GetVTUpdateCount;
--
  FUNCTION GetVTInsertCount(v_tablename IN VARCHAR2) RETURN INTEGER IS
  c_cmdname VARCHAR2(16):='GetVTInsertCount';
  v_count   INTEGER;
  v_sql     VARCHAR2(64);
  BEGIN
    v_sql:='SELECT COUNT(1) FROM '||v_tablename||'_HIST WHERE WM_OPTYPE=''I''';
    EXECUTE IMMEDIATE v_sql INTO v_count;
    RETURN v_count;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,v_sql,v_tablename,sqlcode,sqlerrm);
  END GetVTInsertCount;
--
  FUNCTION GetVTDeleteCount(v_tablename IN VARCHAR2) RETURN INTEGER IS
  c_cmdname VARCHAR2(16):='GetVTDeleteCount';
  v_count   INTEGER;
  v_sql     VARCHAR2(64);
  BEGIN
    v_sql:='SELECT COUNT(1) FROM '||v_tablename||'_HIST WHERE WM_OPTYPE=''D''';
    EXECUTE IMMEDIATE v_sql INTO v_count;
    RETURN v_count;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,v_sql,v_tablename,sqlcode,sqlerrm);
  END GetVTDeleteCount;
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
-- -----------------------------------------------------------------------
END GTMUTIL;
/
SET SERVEROUTPUT ON;
SHOW ERRORS
PROMPT **..............................................................**;
PROMPT **           IMPORTANT INFORMATION PLEASE READ THIS!            **;
PROMPT **..............................................................**;
PROMPT ** -> Any errors occurring above may leave package unusable. **;
SET TERMOUT OFF
GRANT EXECUTE ON GDOSYS.GTMUTIL TO PUBLIC;
SET TERMOUT ON
PROMPT ** -> Execute Privilege on GTMUTIL Package Granted to PUBLIC.**;
SET TERMOUT OFF
DROP PUBLIC SYNONYM GTMUTIL;
CREATE PUBLIC SYNONYM GTMUTIL FOR GDOSYS.GTMUTIL;
SET TERMOUT ON
PROMPT ** -> : Public synonym GTMUTIL created.                        **; 
PROMPT **..............................................................**;
PROMPT ** To turn on package messages, enter: SET SERVEROUTPUT ON      **;  
PROMPT ** For version information, enter:     EXEC GTMUTIL.VERSION     **;
PROMPT ** For Online Help, enter:             EXEC GTMUTIL.HELPME      **;
PROMPT ** ***************   Installation completed.  ***************** **;
PROMPT ******************************************************************;
EXEC GTMUTIL.VERSION;
-- -----------------------------------------------------------------------
