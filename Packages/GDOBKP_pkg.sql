-- ------------------------------------------------------------------------------------------------------------------
-- @GDOBKP_pkg
-- GDOSYS Metadata Backup Package
-- Oracle Object Model Procedures and Functions for for use in backup and restore of GeoMedia's GDOSYS metadata.
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
-- ------------------------------------------------------------------------------------------------------------------
-- DISCLAIMER:
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
-- FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
-- DAMAGES (NOT LIMITED TO THE LOSS OF USE, DATA, OR PROFITS, OR BUSINESS INTERRUPTION HOWEVER
-- CAUSED INCLUDING, AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
-- OR TORT, INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
-- EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- ------------------------------------------------------------------------------------------------------------------
-- INSTALLATION INSTRUCTIONS:
-- This package provides procedures to backup and restore GDOSYS metadata for a given schema. 
-- Caveats:
--   o This package is supported via email only.
--   o Operation requires Oracle 11G or later. 
--   o AFM Metadata is NOT supported, use Feature Model for that.
--   o Existing GDOSYS structure must match version in exported metadata.
--   o Only Core GDOSYS metadata is exported. Metadata for industry applications
--     will not be included in the backup.
--
-- Run script as system DBA after the creation of the GDOSYS metadata schema.  
-- The GOOM package is required in order to run this package and must be intalled first.
-- The GDOBKP package is loaded into GDOSYS and is accessed via a GDOBKP Public synonym.
-- Any user account that makes use  of this package must be explicitly granted the following 
-- privileges:
-- GRANT CREATE TABLE TO <user>;
-- GRANT CREATE SEQUENCE TO <user>;
-- 
-- -------------------------------------------------------------------------------------------------
-- Release History:
-- 01/11/2006  Released to WEB.
-- 04/10/2009  Released to WEB.
-- 12/03/2009  Released to WEB.
-- 04/30/2010  Released to WEB.
-- 03/15/2012  Released to WEB.
-- 07/15/2012  Released to WEB.
-- 07/15/2014  Released to WEB.
-- 07/15/2015  Released to WEB
-- 07/15/2016  Released to WEB
--
-- Changes since last release:
-- 03/26/2015  Bug Fixes and general cleanup.
-- 04/01/2015  Fixed secuity related issues with Open Cursor.
-- 05/01/2015  Tested with Oracle 12C and GM 2015.
-- 07/18/2016  Minor header and other changes.
-- -------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------
PROMPT ******************************************************************;
PROMPT **                    GDOBKP PACKAGE 2016                       **;
PROMPT **        GDOSYS Metadata Backup Utility Installation           **;
PROMPT **                                                              **;
PROMPT ** This package provides procedures to backup/restore GDOSYS    **;
PROMPT ** metadata for a specified schema.                             **;
PROMPT **..............................................................**;
PROMPT **             IMPORTANT INSTALLATION INFORMATION               **;
PROMPT ** -> Oracle 11G or later is REQUIRED                           **;
PROMPT ** -> The GOOM package is required and must be installed first. **;
PROMPT ** -> Installation requires a DBA connection.  The GDOSYS       **;
PROMPT **    schema is the default installation location.              **;
PROMPT ** -> Oracle 12C has tightened security.  You will need to      **;
PROMPT **    GRANT INHERIT PRIVILEGES ON USER <user> TO GDOSYS;        **;
PROMPT **    where <user> is your DBA user account (like SYSTEM).      **;
PROMPT **..............................................................**;
-- -----------------------------------------------------------------------
-- Package declartions
CREATE OR REPLACE PACKAGE GDOSYS.GDOBKP AUTHID CURRENT_USER IS
  -- Main Procedures
  PROCEDURE VERSION;
  PROCEDURE HELPME;
  -- Save Metadata to local schema prior to export of schema.
  PROCEDURE SaveGDOSYSMetadata   (v_owner IN VARCHAR2 DEFAULT USER);
  -- Restore metadata from local schema after import of data.
  PROCEDURE RestoreGDOSYSMetadata(v_owner IN VARCHAR2 DEFAULT USER);
  --
  -- Utility Procedures
  PROCEDURE DeleteGDOSYSMetadata (v_owner IN VARCHAR2 DEFAULT USER, v_respn IN BOOLEAN DEFAULT TRUE);
  PROCEDURE DeleteBackupTables   (v_owner IN VARCHAR2 DEFAULT USER);
  PROCEDURE SaveGCoordSystem     (v_owner IN VARCHAR2 DEFAULT USER);
  PROCEDURE RestoreGCoordSystem  (v_owner IN VARCHAR2 DEFAULT USER);
  PROCEDURE VerifyGDOSYSBkp      (v_owner IN VARCHAR2 DEFAULT USER);
  PROCEDURE VerifySequenceOwner  (v_owner IN VARCHAR2 DEFAULT USER);
  PROCEDURE VerifyLibraryObjectTypeRef;
END;
/
show errors
----------------------------------------------------------------------------
CREATE OR REPLACE PACKAGE BODY GDOSYS.GDOBKP IS
-- -------------------------------------------------------------------------
 
 -- Main data collection cursor 
 CURSOR GetSchemaObjects ( v_owner VARCHAR2 DEFAULT USER ) is
         SELECT object_name
           FROM all_objects
          WHERE owner=v_owner
            AND OBJECT_TYPE IN ('TABLE','VIEW')
            AND OBJECT_NAME NOT LIKE 'RDT%'
            AND OBJECT_NAME NOT LIKE 'MDRT_%'
            AND OBJECT_NAME NOT LIKE 'MDRT%'
            AND OBJECT_NAME NOT LIKE '%$'
            AND OBJECT_NAME NOT IN ('GDOSYS_BKP', 'GCOORDSYS_BKP', 'GCOORDSYS_DEF', 'GOOM_LOG', 'BKP_SDO_GEOM_METADATA');
  --
  -- Exceptions and error messages
  --
  e_NoGDOSYSMetadata    EXCEPTION;
  e_TableNotFound       EXCEPTION;
  e_TabOwnerError       EXCEPTION;
  e_NoDataFound         EXCEPTION;
  e_InvalidGDOType      EXCEPTION;
  e_NoPrivileges        EXCEPTION;
  PRAGMA EXCEPTION_INIT(e_TableNotFound, -942);
  PRAGMA EXCEPTION_INIT(e_NoPrivileges, -1031);
  PRAGMA EXCEPTION_INIT(e_NoDataFound,    100);
  -- Exception Responses
  c_msgNoDataFound      CONSTANT VARCHAR2(70) := 'Specified data could not be found: '                ;
  c_msgTableNotFound    CONSTANT VARCHAR2(55) := 'The specified table or view could not be found: '   ;
  c_msgTabOwnerError    CONSTANT VARCHAR2(55) := 'Only the owner of the table is allowed to do this: ';
  c_msgNoGDOSYSMetadata CONSTANT VARCHAR2(55) := 'No GDOSYS Metadata was found for this user: '       ;
  c_msgInvalidGDOType   CONSTANT VARCHAR2(55) := 'The GDOTableType is not valid for this operation.'  ;
  c_msgNoPrivileges     CONSTANT VARCHAR2(55) := 'Insufficient privileges for this operation.'        ;
  -- Other error messages
  c_msgStart            CONSTANT VARCHAR2(55) := 'Process Started for: '                              ;
  c_msgComplete         CONSTANT VARCHAR2(55) := 'Process Complete for: '                             ;
  c_msgMetadataSaved    CONSTANT VARCHAR2(55) := 'Metadata saved in: '                                ;
  -- Error tags
  c_msgError            CONSTANT VARCHAR2(11) := 'ERROR in '   ;
  c_msgWarning          CONSTANT VARCHAR2(11) := 'WARNING in ' ;
  c_msgFailed           CONSTANT VARCHAR2(9)  := 'FAILED: '    ;
  c_msgVerified         CONSTANT VARCHAR2(10) := 'VERIFIED: '  ;
  c_cmdType             CONSTANT VARCHAR2(8)  := 'GDOBKP '     ;

  ----------------------------------------------------------------------------------------
  -- Global Declarations -- PLEASE DO NOT CHANGE THESE
  ----------------------------------------------------------------------------------------

  c_pkg_version CONSTANT VARCHAR2(10):= '2016.003';
  c_pkg_date    CONSTANT VARCHAR2(10):= '08/01/2016';
  -- -------------------------------------------------------------------------
  PROCEDURE VERSION IS
    c_support     CONSTANT VARCHAR2(14) := 'Via Email Only';
    c_emailpath   CONSTANT VARCHAR2(29) := 'Chuck.Woodbury@hexagonsi.com';
    c_onlinehelp  CONSTANT VARCHAR2(18) := 'EXEC GDOBKP.HELPME';
    i_width       PLS_INTEGER           :=66;
  BEGIN
    GOOM.DblLine( i_width );
    GOOM.TitleLine('Hexagon Safety and Infrastructure/Hexagon Geospatial', i_width,'**');
    GOOM.TitleLine('GDOSYS Metadata Backup Package', i_width ,'**');
    GOOM.TitleLine('GDOBKP PKG', i_width ,'**');
    GOOM.DblLine( i_width );
    GOOM.TitleLine('Author: Chuck Woodbury, Senior Technical Consultant', i_width,'**');
    GOOM.TitleLine('Hexagon Technology Services', i_width,'**');
    GOOM.DblLine( i_width );
    GOOM.Response('Version', c_pkg_version, i_width,TRUE);
    GOOM.Response('Date', c_pkg_date, i_width,TRUE);
    GOOM.Response('Bug Reports and Support', c_support, i_width,TRUE);
    GOOM.Response('Email', c_emailpath, i_width,TRUE);
    GOOM.Response('Online Help', c_onlinehelp, i_width,TRUE);
    GOOM.DblLine( i_width );
  END VERSION;
-- -------------------------------------------------------------------------
-- Online Help
  PROCEDURE HELPME IS
  BEGIN
    GOOM.TitleBlock('GDOBKP PACKAGE PROCEDURES SYNTAX   -   GDOSYS METADATA BACKUP UTILITIES');
    GOOM.TitleLine('Main Procedures');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GDOBKP.VERSION                                                            ');
    GOOM.DBMSG('.    Gets the version and date of this package.                                ');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GDOBKP.HELPME                                                             ');
    GOOM.DBMSG('.    Displays package procedures and syntax.                                   ');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GDOBKP.SaveGDOSYSMetadata; or                                             ');
    GOOM.DBMSG('EXEC GDOBKP.SaveGDOSYSMetadata(c_owner);                                       ');
    GOOM.DBMSG('.    Save GDOSYS Metadata for all feature classes in the schema.               ');
    GOOM.DBMSG('.    DBA users can can run this remotely by specifying the schema name.        ');
    GOOM.DBMSG('.    Run this prior to exporting your schema.                                  ');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GDOBKP.RestoreGDOSYSMetadata;                                             ');
    GOOM.DBMSG('.    This MUST be run by the schema owner (sorry, no other way)                ');
    GOOM.DBMSG('.    Restore saved GDOSYS Metadata for all feature classes in the schema.      ');
    GOOM.DBMSG('.    Run this after importing your schema.                                     ');
    GOOM.DashLine;
    GOOM.TitleLine('Utility Procedures');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GDOBKP.DeleteGDOSYSMetadata; or EXEC GDOBKP.DeleteGDOSYSMetadata(c_owner);');
    GOOM.DBMSG('.    DBA users can can run this remotely by specifying the schema name.        ');
    GOOM.DBMSG('.    Deletes gdosys metadata for all the feature classes in the schema.        ');
    GOOM.DBMSG('.    Careful here, this is not recoverable.  Make a backup first.              ');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GDOBKP.DeleteBackupTables; or EXEC GDOBKP.DeleteBackupTables(c_owner).;   ');
    GOOM.DBMSG('.    DBA users can can run this remotely by specifying the schema name.        ');
    GOOM.DBMSG('.    Deletes the tables used by this package to backup metadata.               ');
    GOOM.DblLine;
    GOOM.DBMSG('EXEC GDOBKP.RestoreGCoordSystem; or EXEC GDOBKP.RestoreGCoordSystem(c_owner);'  );
    GOOM.DBMSG('.    DBA users can can run this remotely by specifying the schema name.        ');
    GOOM.DBMSG('.    Restores the coordinate systems stored in GCOORDSYS_BKP.                  ');
    GOOM.DBMSG('.    The default coordinate system, if any, is also restored.                  ');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GDOBKP.SaveGCoordSystem; or EXEC GDOBKP.SaveGCoordSystem(c_owner);        ');
    GOOM.DBMSG('.    DBA users can can run this remotely by specifying the schema name.        ');
    GOOM.DBMSG('.    Backs up just the coordinate systems in use to GCOORDSYS_BKP.             ');
    GOOM.DBMSG('.    The default coordinate system, if any, is stored in GCOORDSYS_DEF.        ');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GDOBKP.VerifyGDOSYSBkp;  or EXEC GDOBKP.VerifyGDOSYSBkp(c_owner);         ');
    GOOM.DBMSG('.    DBA users can can run this remotely by specifying the schema name.        ');
    GOOM.DBMSG('.    Verify the backup.  Compares existing metadata to the backup              ');
    GOOM.DBMSG('.    and checks that the counts match.                                         ');
    GOOM.DBMSG('.    Always run after EXEC GDOBKP.SaveGDOSYSMetadata;                          ');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GDOBKP.VerifySequenceOwner; or EXEC GDOBKP.VerifySequenceOwner(c_owner);  ');
    GOOM.DBMSG('.    DBA users can can run this remotely by specifying the schema name.        ');
    GOOM.DBMSG('.    Verify the ownership of sequences.  Compares sequence ownership in        ');
    GOOM.DBMSG('-    in the schema with those in GFIELDMAPPING and corrects any problem.       ');
    GOOM.DBMSG('.    Run this after EXEC GDOBKP.RestoreGDOSYSMetadata;                         ');
    GOOM.DotLine;
    GOOM.DBMSG('EXEC GDOBKP.VerifyLibraryObjectTypeRef;');
    GOOM.DBMSG('.    Verify the ownership of tables in the tablename field of the              ');
    GOOM.DBMSG('.    LibraryObjectType table.  This only works for Library schemas.            ');
    GOOM.DblLine;
  END HELPME;
-- ----------------------------------------------------------------------------------------
-- Utility Procedures used by other commands.
--
  PROCEDURE BkpResponse( v_operation IN VARCHAR2, v_results IN VARCHAR2, v_pad IN INTEGER DEFAULT 46, v_sep IN VARCHAR2 DEFAULT '.') AS
  BEGIN
    GOOM.DBMSG(RPAD( v_operation, v_pad, v_sep)||' '|| v_results);
  END;
  --
  -- Verify input/output result count and write results.
  PROCEDURE VerifyCount( v_INcount IN PLS_INTEGER, v_OUTCount IN PLS_INTEGER, v_GDOTable IN VARCHAR2) IS
    c_cmdname       CONSTANT VARCHAR2(11) := 'VerifyCount';
    c_gdomsg        CONSTANT VARCHAR2(11) := 'GDOSYS:';
    c_oramsg        CONSTANT VARCHAR2(11) := 'ORACLE:';
    --
    v_msg           VARCHAR2(10);
    v_bkptable      VARCHAR2(30);
  BEGIN
    IF v_GDOTable = GOOM.GetGDOTablename('GCoordSystemTable') THEN
      v_bkptable := 'GCOORDSYS_BKP';
      v_msg      := c_gdomsg;
    ELSIF v_GDOTable = GOOM.GetGDOTablename('GParameters') THEN
      v_bkptable := 'GCOORDSYS_DEF';
      v_msg      := c_gdomsg;
    ELSIF v_GDOTable = 'USER_SDO_GEOM_METADATA' THEN
      v_bkptable := 'BKP_SDO_GEOM_METADATA' ;
      v_msg := c_oramsg;
    ELSE
      v_bkptable :='GDOSYS_BKP';
      v_msg      := c_gdomsg;
    END IF;
    IF ABS(v_INCount - v_OUTCount) = 0 THEN
      BkpResponse(c_msgVerified || v_GDOTable, RPAD(v_msg || v_INCount, 17) || v_bkptable||':' || v_OUTCount);
    ELSE
      BkpResponse(c_msgFailed|| v_GDOTable, RPAD(v_msg || v_INCount, 17) || v_bkptable||':' || v_OUTCount,46,'*');
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, 'Verify:' || v_INCount - v_OUTCount, v_GDOTable, SQLCODE, SQLERRM);
  END VerifyCount;
  --
  -- Verify/Repair Sequence Ownership after metadata import
  PROCEDURE VerifySequenceOwner( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(20) := 'VerifySequenceOwner';
    c_feedback1     CONSTANT VARCHAR2(32) := 'Sequence references repaired: ';
    c_feedback2     CONSTANT VARCHAR2(32) := 'No Invalid Sequence References.';
    c_err           CONSTANT VARCHAR2(15) := 'Invalid Count: ';
    --
    v_count         PLS_INTEGER           := 0;
  BEGIN
    -- Check the sequences owned by user
    SELECT COUNT(*)
      INTO v_count
      FROM GDOSYS.GFIELDMAPPING
     WHERE OWNER = v_owner
       AND SEQUENCE_OWNER <> v_owner
       AND SEQUENCE_NAME IN (SELECT SEQUENCE_NAME FROM ALL_SEQUENCES WHERE OWNER =  v_owner);
    -- Compare these to sequences assigned in GDOSYS metadata
    IF v_count > 0 THEN
      UPDATE GDOSYS.GFIELDMAPPING
         SET SEQUENCE_OWNER = v_owner
       WHERE OWNER = v_owner
         AND SEQUENCE_OWNER <> v_owner
         AND SEQUENCE_NAME IN (SELECT SEQUENCE_NAME FROM ALL_SEQUENCES WHERE owner = v_owner);
      BkpResponse(c_cmdname, c_feedback1 || v_count,40);
    ELSE
      BkpResponse(c_cmdname, c_feedback2,40);
    END IF;
    COMMIT;
    GOOM.DashLine;
  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      GOOM.REPORT_ERROR(c_cmdname||c_msgError, c_err||v_count, v_owner, SQLCODE, SQLERRM);
  END VerifySequenceOwner;
  --
  -- Verify/Repair LibraryObjectType Ownership after metadata import
  PROCEDURE VerifyLibraryObjectTypeRef IS
    c_cmdname       CONSTANT VARCHAR2(30) := 'VerifyLibraryObjectTypeRef';
    c_feedback1     CONSTANT VARCHAR2(39) := 'LibraryObjectType references repaired: ';
    c_feedback2     CONSTANT VARCHAR2(40) := 'No Invalid LibraryObjectType References.';
    c_feedback3     CONSTANT VARCHAR2(40) := 'Skipped, not a library schema.';
    --
    v_query        VARCHAR2(2048);
    i_cur          PLS_INTEGER;
    v_dum          PLS_INTEGER;
    v_object       VARCHAR2(61);
    v_ownertable   VARCHAR2(61);
    v_sql          VARCHAR2(1024);
    v_count        PLS_INTEGER := 0;
  BEGIN      
    IF GOOM.chkTable( USER ||'.LIBRARYOBJECTTYPES') THEN
      v_query := 'SELECT TABLENAME FROM LibraryObjectTypes WHERE TABLENAME IS NOT NULL';
      i_cur := DBMS_SQL.OPEN_CURSOR;
      DBMS_SQL.PARSE( i_cur, v_query, DBMS_SQL.NATIVE);
      DBMS_SQL.DEFINE_COLUMN( i_cur, 1, v_object, 61);
      v_Dum := DBMS_SQL.EXECUTE( i_cur );
      LOOP   
        IF (DBMS_SQL.FETCH_ROWS( i_cur ) > 0) THEN
          DBMS_SQL.COLUMN_VALUE( i_cur, 1, v_object);
          IF GOOM.SplitOwnerObject( v_object,'OWNER' ) <> USER THEN
            BEGIN
            v_ownertable := GOOM.GetOwnerObject(GOOM.SplitOwnerObject( v_object,'TABLE' ));
            v_sql        := 'UPDATE '||USER||'.LIBRARYOBJECTTYPES SET TABLENAME='''|| v_ownertable ||''' WHERE TABLENAME='''|| v_object||'''';
            EXECUTE IMMEDIATE (v_sql);
            v_count := v_count + 1;
            EXCEPTION
              WHEN OTHERS THEN
              ROLLBACK;
            END;
            COMMIT;
          END IF;
        ELSE
          EXIT;
        END IF;      
      END LOOP; 
      IF v_count = 0 THEN
        BkpResponse(c_cmdname, c_feedback2,40); 
      ELSE
        BkpResponse(c_cmdname, c_feedback1 || v_count, 40); 
      END IF; 
    ELSE
      BkpResponse(c_cmdname, c_feedback3, 40); 
    END IF;
    IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
       SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    END IF;
    GOOM.DashLine;
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
        SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      ROLLBACK;
      GOOM.REPORT_ERROR(c_cmdname||c_msgError, v_ownertable, v_sql, SQLCODE, SQLERRM);
  END VerifyLibraryObjectTypeRef;
  --
  -- Verify the exstence of GDOSYS metadata entries for each table in the schema.
  -- Each metadata table is checked to see if it contains entries for the specified owner.table.
  FUNCTION GDOMetadataExists( v_GDOTableType IN VARCHAR2, v_ownertable in VARCHAR2) RETURN BOOLEAN IS
  c_cmdname     CONSTANT VARCHAR2(17) :='GDOMetadataExists';
  --
  v_sql         VARCHAR2(255) :=NULL;
  v_count       PLS_INTEGER   :=0;
  v_owner       VARCHAR2(30)  :=NULL;
  v_table       VARCHAR2(30)  :=NULL;
  v_GDOTable    VARCHAR2(61)  :=NULL;
  BEGIN
    v_owner     := GOOM.SplitOwnerObject(v_ownertable, 'OWNER');
    v_table     := GOOM.SplitOwnerObject(v_ownertable, 'TABLE');
    v_GDOTable  := GOOM.GetGDOTableName(v_GDOTableType);
    --
    CASE v_GDOTableType
      WHEN 'INGRFeatures' THEN              -- GFeatures
        v_sql := 'SELECT COUNT(1) FROM GDOSYS.GFEATURESBASE WHERE FEATURENAME=:vownertable';
        EXECUTE IMMEDIATE v_sql INTO v_count USING v_ownertable;
      WHEN 'INGRFieldLookup' THEN           -- FIELDLOOKUP, GEOMETRYPROPERTIES, ATTRIBUTEPROPERTIES
        v_sql := 'SELECT COUNT(1) FROM '|| v_GDOTable ||' WHERE FEATURENAME=:vownertable';
        EXECUTE IMMEDIATE v_sql INTO v_count USING v_ownertable;
      WHEN 'INGRPickLists' THEN            -- GPickLists
        v_sql := 'SELECT COUNT(1) FROM '|| v_GDOTable ||' WHERE FEATURENAME=:vownertable';
        EXECUTE IMMEDIATE v_sql INTO v_count USING v_ownertable;
      WHEN 'GOracleFieldMapping' THEN       --GFieldMapping
        v_sql := 'SELECT COUNT(1) FROM '|| v_GDOTable ||' WHERE OWNER=:vowner and TABLE_NAME=:vtable';
        EXECUTE IMMEDIATE v_sql INTO v_count USING v_owner, v_table;
      WHEN 'GOracleIndexColumns' THEN       -- GIndex_columns
        v_sql := 'SELECT COUNT(1) FROM '|| v_GDOTable ||' WHERE OWNER=:vowner AND OBJECT_NAME=:vobject';
        EXECUTE IMMEDIATE v_sql INTO v_count USING v_owner, v_table;
      WHEN 'LibraryTables' THEN             -- GIndex_columns
        v_sql := 'SELECT COUNT(1) FROM GDOSYS.LIBRARYTABLESBASE WHERE TABLENAME=:vownertable';
        EXECUTE IMMEDIATE v_sql INTO v_count USING v_ownertable;
      ELSE
        RAISE e_InvalidGDOType;
    END CASE;
    IF v_count > 0 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END IF;
  EXCEPTION
    WHEN e_InvalidGDOType THEN
      GOOM.REPORT_ERROR(c_cmdname, v_GDOTableType, v_ownertable, SQLCODE, SQLERRM);
      RETURN FALSE;
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_owner, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END GDOMetadataExists;
-- ----------------------------------------------------------------------------------------
-- This section creates the GDOSYS_BKP table used by the backup prccess.  It also contains the
-- procedures to populate the GDOSYS_BKP table.
  --
  -- Create the Backup table GDOSYS_BKP.  If it exists, delete and re-create it.
  PROCEDURE Create_GDOSYS_BKP_Table( v_owner IN VARCHAR2 DEFAULT USER ) IS
   c_cmdname VARCHAR2(32) := 'Create_GDOSYS_BKP_Table';
   v_debug   VARCHAR2(255):= 'INIT';
  BEGIN
    IF GOOM.chkTable( v_owner||'.GDOSYS_BKP') THEN
      v_debug := 'The backup table exists, drop it.';
      EXECUTE IMMEDIATE 'DROP TABLE '||v_owner||'.GDOSYS_BKP PURGE';
    END IF;
    IF GOOM.chkSequence( v_owner||'.GDOSYS_BKP_SEQ') THEN
      v_debug := 'The sequence exists, drop it.';
      EXECUTE IMMEDIATE 'DROP SEQUENCE '||v_owner||'.GDOSYS_BKP_SEQ';
    END IF;
    -- Now create the backup table.
    EXECUTE IMMEDIATE 'CREATE TABLE '|| v_owner ||'.GDOSYS_BKP(PID INTEGER PRIMARY KEY, GDOSYS_TABLE VARCHAR2(39),FEATURECLASS VARCHAR2(32), INSERT_SQL VARCHAR2(2048))';
    EXECUTE IMMEDIATE 'CREATE SEQUENCE '|| v_owner ||'.GDOSYS_BKP_SEQ START WITH 1 INCREMENT BY 1 CACHE 50';
    v_debug := 'Created New GDOSYS_BKP Table.';
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname||c_msgError, v_owner, v_debug, SQLCODE, SQLERRM);
  END Create_GDOSYS_BKP_Table;
  --
  -- Insert Backup Infomation into the GDOSYS_BKP table
  PROCEDURE WRITE_BKP(v_gdotable IN VARCHAR2, v_featclass IN VARCHAR2, v_sql IN VARCHAR2, v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname VARCHAR2(32)   := 'WRITE_BKP';
    v_insert  VARCHAR2(2048) := NULL;
  BEGIN
    IF NOT GOOM.chkTable( v_owner||'.GDOSYS_BKP' ) THEN
      RAISE e_TableNotFound;
    END IF;
    -- If the backup table exists, load it.
    v_insert := 'INSERT INTO '|| v_owner ||'.GDOSYS_BKP VALUES('|| v_owner ||'.GDOSYS_BKP_SEQ.nextval,''' || v_gdotable || ''',''' || v_featclass || ''',''' || v_sql || ''')';
    EXECUTE IMMEDIATE v_insert;
    COMMIT;
  EXCEPTION
    WHEN e_TableNotFound THEN
      BkpResponse( c_cmdname || c_msgError, c_msgTableNotFound || v_owner||'.GDOSYS_BKP');
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR( c_cmdname || c_msgError, v_insert, v_sql, SQLCODE, SQLERRM);
  END WRITE_BKP;
-- ----------------------------------------------------------------------------------------
-- This section contains the internal backup procedures for each of the primary GDOSYS 
-- metadata tables, they are not PUBLIC procedures and should not be called individually.
-- Each procedure looks at the GALIASTABLE to determine the actual metadata table name.
--
  -- AttributeProperties
  PROCEDURE BkpAttrProps( v_ownertable IN VARCHAR2, v_fieldname IN VARCHAR2) IS
    c_cmdname      VARCHAR2(32) := 'BkpAttrProps';
    c_sep          VARCHAR2(9) := ''''',''''';
    --
    v_attrprops    VARCHAR2(64);
    v_gfieldlookup VARCHAR2(64);
    v_insert       VARCHAR2(512);
    v_sql          VARCHAR2(2048);
    v_query        VARCHAR2(2048);
    i_cur          PLS_INTEGER;
    v_dum          PLS_INTEGER;
    v_IsKeyField   VARCHAR2(38)  := NULL;
    v_FieldDesc    VARCHAR2(255) := NULL;
    v_FieldFormat  VARCHAR2(255) := NULL;
    v_FieldType    NUMBER        := NULL;
    v_IsDisp       NUMBER        := NULL;
    v_FieldPre     NUMBER        := NULL;
    v_owner        VARCHAR2(30)  := NULL;
    v_table        VARCHAR2(30)  := NULL;
    v_debug        VARCHAR2(32)  := 'INIT';
    --
  BEGIN
    -- Collect user and table metadata table information
    v_owner        :=GOOM.SplitOwnerObject(v_ownertable,'OWNER');
    v_table        :=GOOM.SplitOwnerObject(v_ownertable,'TABLE');
    v_attrprops    := GOOM.GetGDOTableName('INGRAttributeProperties');
    v_gfieldlookup := GOOM.GetGDOTableName('INGRFieldLookup');
    -- Construct first half of insert
    v_insert       := 'INSERT INTO ' || v_attrprops ||
                      '(ISKEYFIELD, FIELDDESCRIPTION, FIELDFORMAT, FIELDTYPE, ISFIELDDISPLAYABLE, FIELDPRECISION, INDEXID) select ';
    -- Collect metadata values
    v_query  := 'SELECT ISKEYFIELD, FIELDDESCRIPTION, FIELDFORMAT, FIELDTYPE, ISFIELDDISPLAYABLE, FIELDPRECISION FROM ' || v_attrprops ||
                ' WHERE INDEXID in (SELECT INDEXID FROM ' || v_gfieldlookup || ' WHERE FEATURENAME=''' || v_ownertable ||
                ''' AND FIELDNAME=''' || v_fieldname || ''')';
    i_cur := DBMS_SQL.OPEN_CURSOR;
    DBMS_SQL.PARSE( i_cur, v_query, DBMS_SQL.NATIVE);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 1, v_IsKeyField, 38);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 2, v_FieldDesc, 255);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 3, v_FieldFormat, 255);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 4, v_FieldType);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 5, v_IsDisp);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 6, v_FieldPre);
    v_Dum := DBMS_SQL.EXECUTE(i_cur);
    LOOP
      -- Generate the insert statements
      IF (DBMS_SQL.FETCH_ROWS(i_cur) > 0) THEN
        DBMS_SQL.COLUMN_VALUE(i_cur, 1, v_IsKeyField);
        DBMS_SQL.COLUMN_VALUE(i_cur, 2, v_FieldDesc);
        DBMS_SQL.COLUMN_VALUE(i_cur, 3, v_FieldFormat);
        DBMS_SQL.COLUMN_VALUE(i_cur, 4, v_FieldType);
        DBMS_SQL.COLUMN_VALUE(i_cur, 5, v_IsDisp);
        DBMS_SQL.COLUMN_VALUE(i_cur, 6, v_FieldPre);
        -- In the following adjustment to v_FieldDesc, single quote marks are replaced
        -- with twin single quote marks.  This is necessary to get the right effect when we run Restore.
        -- The problem is that we may cause the string to exceed the 255 char limit for the export field size.
        -- In that case, we truncate the string to 255 characters.  But this introduces another risk, that of
        -- truncating a pair of single quotes and causing a string delimiter error during restore.  Risk is low
        -- but the error is still there.
        v_FieldDesc := SUBSTR(REPLACE(v_FieldDesc, CHR(39), CHR(39) || CHR(39) || CHR(39) || CHR(39)), 1, 255);
        -- Construct the insert statement.
        v_sql       := v_insert || v_IsKeyField || ',''''' || v_FieldDesc || c_sep || v_FieldFormat || c_sep || v_FieldType || c_sep || v_IsDisp ||
                       c_sep || v_FieldPre || ''''',INDEXID  FROM ' || v_gfieldlookup || ' where FEATURENAME =USER||''''.''''||''''' || v_table ||
                       ''''' AND FIELDNAME=''''' || v_fieldname || '''''';
        -- Write the statement to the backup table.
        WRITE_BKP(v_attrprops, v_table, v_sql, v_owner);
      ELSE
        EXIT;
      END IF;
    END LOOP;
    IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
       SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      GOOM.REPORT_ERROR(c_cmdname, v_query, v_debug, SQLCODE, SQLERRM);
  END BkpAttrProps;
  --
  -- GeometryProperties
  PROCEDURE BkpGeomProps( v_ownertable IN VARCHAR2, v_fieldname IN VARCHAR2) IS
    c_cmdname      VARCHAR2(32) := 'BkpGeomProps';
    c_sep          VARCHAR2(9) := ''''',''''';
    --
    v_geomprops    VARCHAR2(64);
    v_gfieldlookup VARCHAR2(64);
    v_insert       VARCHAR2(512);
    v_sql          VARCHAR2(2048);
    v_query        VARCHAR2(2048);
    i_cur          PLS_INTEGER;
    v_dum          PLS_INTEGER;
    v_PGeomFlag    VARCHAR2(38)  := NULL;
    v_GeomType     VARCHAR2(38)  := NULL;
    v_GCoordID     VARCHAR2(255) := NULL;
    v_FieldDesc    VARCHAR2(255) := NULL;
    v_owner        VARCHAR2(30)  := NULL;
    v_table        VARCHAR2(30)  := NULL;
    v_debug        VARCHAR2(32)  := 'INIT';
    --
  BEGIN
    -- Collect user and table metadata table information
    v_owner        := GOOM.SplitOwnerObject(v_ownertable,'OWNER');
    v_table        := GOOM.SplitOwnerObject(v_ownertable,'TABLE');
    v_geomprops    := GOOM.GetGDOTableName('INGRGeometryProperties');
    v_gfieldlookup := GOOM.GetGDOTableName('INGRFieldLookup');
    v_insert       := 'INSERT INTO ' || v_geomprops || '(PRIMARYGEOMETRYFLAG, GEOMETRYTYPE, GCOORDSYSTEMGUID, FIELDDESCRIPTION, INDEXID) select ''''';
    -- Collect metadata values
    v_query  := 'SELECT PRIMARYGEOMETRYFLAG, GEOMETRYTYPE, GCOORDSYSTEMGUID, FIELDDESCRIPTION FROM ' || v_geomprops ||
                ' WHERE INDEXID IN (SELECT INDEXID FROM ' || v_gfieldlookup || ' WHERE FEATURENAME=''' || v_ownertable ||
                ''' AND FIELDNAME=''' || v_fieldname || ''')';
    i_cur := DBMS_SQL.OPEN_CURSOR;
    DBMS_SQL.PARSE( i_cur, v_query, DBMS_SQL.NATIVE);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 1, v_PGeomFlag, 38);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 2, v_GeomType, 38);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 3, v_GCoordID, 255);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 4, v_FieldDesc, 255);
    v_Dum := DBMS_SQL.EXECUTE(i_cur);
    LOOP
      -- Generate the insert statements
      IF (DBMS_SQL.FETCH_ROWS(i_cur) > 0) THEN
        DBMS_SQL.COLUMN_VALUE(i_cur, 1, v_PGeomFlag);
        DBMS_SQL.COLUMN_VALUE(i_cur, 2, v_GeomType);
        DBMS_SQL.COLUMN_VALUE(i_cur, 3, v_GCoordID);
        DBMS_SQL.COLUMN_VALUE(i_cur, 4, v_FieldDesc);
        -- In the following adjustment to v_FieldDesc, single quote marks are replaced
        -- with twin single quote marks.  This is necessary to get the right effect when we run Restore.
        -- The problem is that we may cause the string to exceed the 255 char limit for the export field size.
        -- In that case, we truncate the string to 255 characters.  But this introduces another risk, that of
        -- truncating a pair of single quotes and causing a string delimiter error during restore.  Risk is low
        -- but the error is still there.
        v_FieldDesc := SUBSTR(REPLACE(v_FieldDesc, CHR(39), CHR(39) || CHR(39) || CHR(39) || CHR(39)), 1, 255);
        -- Construct the insert statement.
        v_sql       := v_insert || v_PGeomFlag || c_sep || v_GeomType || c_sep || v_GCoordID || c_sep || v_FieldDesc || ''''',INDEXID from ' ||
                       v_gfieldlookup || ' where FEATURENAME =USER||''''.''''||''''' || v_table || ''''' AND FIELDNAME=''''' || v_fieldname ||
                       '''''';
        -- Write the statement to the backup table.
        WRITE_BKP(v_geomprops, v_table, v_sql,v_owner);
      ELSE
        EXIT;
      END IF;
    END LOOP;
    IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
       SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
        SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      GOOM.REPORT_ERROR(c_cmdname, v_query, v_debug, SQLCODE, SQLERRM);
  END BkpGeomProps;
  --
  -- FieldLookup - Calls AttributeProperties and GeometryProperties
  PROCEDURE BkpFieldLookup( v_ownertable IN VARCHAR2) IS
    c_cmdname      VARCHAR2(32) := 'BkpFieldLookup';
    c_sep          VARCHAR2(9)  := ''''',''''';
    --
    v_gfieldlookup VARCHAR2(64);
    v_insert       VARCHAR2(512);
    v_sql          VARCHAR2(2048);
    v_query        VARCHAR2(2048);
    i_cur          PLS_INTEGER;
    v_dum          PLS_INTEGER;
    v_FieldName    VARCHAR2(255) := NULL;
    v_owner        VARCHAR2(30)  := NULL;
    v_table        VARCHAR2(30)  := NULL;
    v_debug        VARCHAR2(32)  := 'INIT';
    --
  BEGIN
    -- Get table and owner information
    v_owner         := GOOM.SplitOwnerObject(v_ownertable,'OWNER');
    v_table         := GOOM.SplitOwnerObject(v_ownertable,'TABLE');
    v_gfieldlookup  := GOOM.GetGDOTableName('INGRFieldLookup');
    -- Construct first half of insert
    v_insert        := 'INSERT INTO ' || v_gfieldlookup || '(INDEXID, FEATURENAME, FIELDNAME ) SELECT GDOSYS.FIELDLOOKUPINDEXID1.NEXTVAL';
    -- Collect metadata values
    v_query  := 'SELECT DISTINCT(FIELDNAME) FROM ' || v_gfieldlookup || ' where FEATURENAME=''' || v_ownertable || '''';
    i_cur := DBMS_SQL.OPEN_CURSOR;
    DBMS_SQL.PARSE( i_cur, v_query, DBMS_SQL.NATIVE);
    dbms_sql.DEFINE_COLUMN(i_cur, 1, v_FieldName, 255);
    v_Dum := DBMS_SQL.EXECUTE(i_cur);
    LOOP
      -- Generate the insert statements
      IF (DBMS_SQL.FETCH_ROWS(i_cur) > 0) THEN
        DBMS_SQL.COLUMN_VALUE(i_cur, 1, v_FieldName);
        v_sql := v_insert || ', USER||''''.''''||''''' || v_table || c_sep || v_FieldName || ''''' from dual';
        WRITE_BKP(v_gfieldlookup, v_table, v_sql,v_owner);
        BkpAttrProps(v_ownertable, v_fieldname); -- Backup AttributeProperties - Requires FieldLookup
        BkpGeomProps(v_ownertable, v_fieldname); -- Backup GeometryProperties  - Requires FieldLookup
        v_debug := 'In Loop';
      ELSE
        EXIT;
      END IF;
    END LOOP;
    IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
       SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
        SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      GOOM.REPORT_ERROR(c_cmdname, v_query, v_debug, SQLCODE, SQLERRM);
  END BkpFieldLookup;
  --
  -- FieldMapping
  PROCEDURE BkpGfieldMapping( v_ownertable IN VARCHAR2) IS
    c_cmdname      VARCHAR2(32) := 'BkpGfieldMapping';
    c_sep          VARCHAR2(9)  := ''''',''''';
    --
    v_fmaptable    VARCHAR2(64);
    v_insert       VARCHAR2(255);
    v_sql          VARCHAR2(2048);
    v_query        VARCHAR2(2048);
    i_cur          PLS_INTEGER;
    v_dum          PLS_INTEGER;
    v_tablename    VARCHAR2(32) := NULL;
    v_columnname   VARCHAR2(32) := NULL;
    v_datatype     NUMBER       := NULL;
    v_datasubtype  NUMBER       := NULL;
    v_csguid       VARCHAR2(38) := NULL;
    v_seqowner     VARCHAR2(32) := NULL;
    v_sequencename VARCHAR2(32) := NULL;
    v_owner        VARCHAR2(30) := NULL;
    v_table        VARCHAR2(30) := NULL;
    v_debug        VARCHAR2(32) := 'INIT';
    --
  BEGIN
    v_owner     := GOOM.SplitOwnerObject(v_ownertable,'OWNER');
    v_table     := GOOM.SplitOwnerObject(v_ownertable,'TABLE');
    v_fmaptable := GOOM.GetGDOTableName('GOracleFieldMapping');
    v_insert    := 'INSERT INTO ' || v_fmaptable || ' select USER,''''';
    -- Collect metadata values
    v_query     := 'SELECT TABLE_NAME,COLUMN_NAME,DATA_TYPE,DATA_SUBTYPE,CSGUID,SEQUENCE_OWNER,SEQUENCE_NAME FROM ' || v_fmaptable ||
                   ' WHERE OWNER = ''' || v_owner || ''' AND TABLE_NAME=''' || v_table || '''';
    i_cur := DBMS_SQL.OPEN_CURSOR;
    DBMS_SQL.PARSE( i_cur, v_query, DBMS_SQL.NATIVE);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 1, v_tablename, 30);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 2, v_columnname, 30);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 3, v_datatype);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 4, v_datasubtype);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 5, v_csguid, 38);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 6, v_seqowner, 30);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 7, v_sequencename, 30);
    v_Dum := DBMS_SQL.EXECUTE(i_cur);
    LOOP
      IF (DBMS_SQL.FETCH_ROWS(i_cur) > 0) THEN
        DBMS_SQL.COLUMN_VALUE(i_cur, 1, v_tablename);
        DBMS_SQL.COLUMN_VALUE(i_cur, 2, v_columnname);
        DBMS_SQL.COLUMN_VALUE(i_cur, 3, v_datatype);
        DBMS_SQL.COLUMN_VALUE(i_cur, 4, v_datasubtype);
        DBMS_SQL.COLUMN_VALUE(i_cur, 5, v_csguid);
        DBMS_SQL.COLUMN_VALUE(i_cur, 6, v_seqowner);
        DBMS_SQL.COLUMN_VALUE(i_cur, 7, v_sequencename);
        v_sql := v_insert || v_tablename || c_sep || v_columnname || c_sep || v_datatype || c_sep || v_datasubtype || c_sep || v_csguid || c_sep ||
                 v_seqowner || c_sep || v_sequencename || ''''' from dual';
        WRITE_BKP(v_fmaptable, v_table, v_sql, v_owner);
      ELSE
        EXIT;
      END IF;
    END LOOP;
    IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
       SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
        SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      GOOM.REPORT_ERROR(c_cmdname, v_query, v_debug, SQLCODE, SQLERRM);
  END BkpGfieldMapping;
  --
  -- GIndex_Columns
  PROCEDURE BkpGIndexColumns( v_ownertable IN VARCHAR2) IS
    c_cmdname     VARCHAR2(32) := 'BkpGIndexColumns';
    c_sep         VARCHAR2(9)  := ''''',''''';
    --
    v_gindextable VARCHAR2(64);
    v_insert      VARCHAR2(255) := NULL;
    v_sql         VARCHAR2(2048):= NULL;
    v_query       VARCHAR2(2048):= NULL;
    i_cur         PLS_INTEGER;
    v_dum         PLS_INTEGER;
    v_count       PLS_INTEGER   := 0;
    v_indexname   VARCHAR2(30)  := NULL;
    v_indextype   VARCHAR2(2)   := NULL;
    v_columnname  VARCHAR2(30)  := NULL;
    v_columnPos   NUMBER        := NULL;
    v_debug       VARCHAR2(32)  := 'INIT';
    v_owner       VARCHAR2(30);
    v_table       VARCHAR2(30);
    --
  BEGIN
    v_owner       := GOOM.SplitOwnerObject(v_ownertable,'OWNER');
    v_table       := GOOM.SplitOwnerObject(v_ownertable,'TABLE');
    v_gindextable := GOOM.GetGDOTableName('GOracleIndexColumns');
    v_insert      := 'INSERT INTO ' || v_gindextable || ' VALUES(USER,''''';
    -- Collect metadata values
    EXECUTE IMMEDIATE 'SELECT COUNT(1) FROM ' || v_gindextable || ' WHERE OBJECT_NAME=''' || v_table || ''''
      INTO v_count;
    IF v_count > 0 THEN
      v_query  := 'SELECT INDEX_NAME,INDEX_TYPE,COLUMN_NAME,COLUMN_POSITION FROM ' || v_gindextable ||
                  ' WHERE OWNER = ''' || v_owner || ''' AND OBJECT_NAME=''' || v_table || '''';
      i_cur := DBMS_SQL.OPEN_CURSOR;
      DBMS_SQL.PARSE( i_cur, v_query, DBMS_SQL.NATIVE);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 1, v_indexname, 30);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 2, v_indextype, 2);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 3, v_columnname, 30);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 4, v_columnPos);
      v_Dum := DBMS_SQL.EXECUTE(i_cur);
      LOOP
        IF (DBMS_SQL.FETCH_ROWS(i_cur) > 0) THEN
          DBMS_SQL.COLUMN_VALUE(i_cur, 1, v_indexname);
          DBMS_SQL.COLUMN_VALUE(i_cur, 2, v_indextype);
          DBMS_SQL.COLUMN_VALUE(i_cur, 3, v_columnname);
          DBMS_SQL.COLUMN_VALUE(i_cur, 4, v_columnPos);
          v_sql := v_insert||v_table||c_sep||v_indexname||c_sep||v_indextype||c_sep||v_columnname||c_sep||v_columnPos||''''')';
          WRITE_BKP(v_gindextable, v_table, v_sql, v_owner);
        ELSE
          EXIT;
        END IF;
      END LOOP;
    IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
       SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    END IF;
    ELSE
      NULL;
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      GOOM.REPORT_ERROR(c_cmdname, v_query, v_debug, SQLCODE, SQLERRM);
  END BkpGIndexColumns;
  --
  --GPickLists
  PROCEDURE BkpGPickList( v_ownertable IN VARCHAR2) IS
    c_cmdname VARCHAR2(32) := 'BkpGIndexColumns';
    c_sep     VARCHAR2(9)  := ''''',''''';
    --
    v_gplisttable          VARCHAR2(64);
    v_insert               VARCHAR2(512);
    v_sql                  VARCHAR2(2048);
    v_query                VARCHAR2(2048);
    i_cur                  PLS_INTEGER;
    v_dum                  PLS_INTEGER;
    v_plowner              VARCHAR2(32);
    v_count                PLS_INTEGER   := 0;
    v_debug                VARCHAR2(32)  :='INIT';
    v_featurename          VARCHAR2(64)  := NULL;
    v_fieldname            VARCHAR2(32)  := NULL;
    v_PicklistTablename    VARCHAR2(32)  := NULL;
    v_ValueFieldname       VARCHAR2(32)  := NULL;
    v_DescriptionFieldname VARCHAR2(255) := NULL;
    v_Filterclause         VARCHAR2(255) := NULL;
    --
  BEGIN
    v_gplisttable   := GOOM.GetGDOTableName('INGRPickLists');
    v_insert        := 'INSERT INTO '||v_gplisttable||'(FEATURENAME, FIELDNAME, PICKLISTTABLENAME, VALUEFIELDNAME,DESCRIPTIONFIELDNAME,FILTERCLAUSE ) select ';
    -- Check for picklists and bkp if it exists.
    EXECUTE IMMEDIATE 'SELECT COUNT(1) FROM ' || v_gplisttable || ' WHERE FEATURENAME=''' || v_ownertable || '''' INTO v_count;
    IF v_count > 0 THEN
      v_query  := 'SELECT FEATURENAME, FIELDNAME, PICKLISTTABLENAME, VALUEFIELDNAME,DESCRIPTIONFIELDNAME, FILTERCLAUSE
                     FROM ' || v_gplisttable ||
                  ' WHERE FEATURENAME=''' || v_ownertable || '''';
      i_cur := DBMS_SQL.OPEN_CURSOR;
      DBMS_SQL.PARSE( i_cur, v_query, DBMS_SQL.NATIVE);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 1, v_featurename, 64);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 2, v_fieldname, 32);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 3, v_PicklistTablename, 64);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 4, v_ValueFieldname, 32);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 5, v_DescriptionFieldname, 255);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 6, v_Filterclause, 255);
      v_Dum := DBMS_SQL.EXECUTE(i_cur);
      LOOP
        IF (DBMS_SQL.FETCH_ROWS(i_cur) > 0) THEN
          DBMS_SQL.COLUMN_VALUE(i_cur, 1, v_featurename);
          DBMS_SQL.COLUMN_VALUE(i_cur, 2, v_fieldname);
          DBMS_SQL.COLUMN_VALUE(i_cur, 3, v_PicklistTablename);
          DBMS_SQL.COLUMN_VALUE(i_cur, 4, v_ValueFieldname);
          DBMS_SQL.COLUMN_VALUE(i_cur, 5, v_DescriptionFieldname);
          DBMS_SQL.COLUMN_VALUE(i_cur, 6, v_Filterclause);
          v_plowner:=GOOM.SplitOwnerObject(v_PicklistTablename,'OWNER');
          v_featurename:=GOOM.SplitOwnerObject(v_featurename,'TABLE');
          IF v_plowner = USER THEN -- If current user owns picklist table then allow ownership change.
            v_PicklistTablename:=GOOM.SplitOwnerObject(v_PicklistTablename,'TABLE');
            v_FilterClause := SUBSTR(REPLACE(v_FilterClause, CHR(39), CHR(39) || CHR(39) || CHR(39) || CHR(39)), 1, 255);
            v_sql          := v_insert ||'USER||''''.' || v_featurename || c_sep || v_fieldname ||''''', USER||''''.' || v_PicklistTablename || c_sep || v_ValueFieldname || c_sep ||
                              v_DescriptionFieldname || c_sep || v_Filterclause || ''''' from dual';
          ELSE -- Current user does not own picklist table.
            v_FilterClause := SUBSTR(REPLACE(v_FilterClause, CHR(39), CHR(39) || CHR(39) || CHR(39) || CHR(39)), 1, 255);
            v_sql          := v_insert ||'USER||''''.' || v_featurename || c_sep || v_fieldname || c_sep || v_PicklistTablename || c_sep || v_ValueFieldname || c_sep ||
                              v_DescriptionFieldname || c_sep || v_Filterclause || ''''' from dual';
          END IF;
          WRITE_BKP(v_gplisttable, v_featurename, v_sql, v_plowner);
        ELSE
          EXIT;
        END IF;
      END LOOP;
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
    ELSE
      NULL;
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
        SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      GOOM.REPORT_ERROR(c_cmdname, v_query, v_debug, SQLCODE, SQLERRM);
  END BkpGPickList;
  --
  -- GFeatures
  PROCEDURE BkpGFeatures( v_ownertable IN VARCHAR2) IS
    c_cmdname    VARCHAR2(32) := 'BkpGFeatures';
    c_sep        VARCHAR2(9) := ''''',''''';
    --
    v_gfeatures  VARCHAR2(64);
    v_insert     VARCHAR2(512);
    v_sql        VARCHAR2(2048);
    v_query      VARCHAR2(2048);
    i_cur        PLS_INTEGER;
    v_dum        PLS_INTEGER;
    v_GeomType   NUMBER;
    v_PGeomField VARCHAR2(255) := NULL;
    v_FeatDesc   VARCHAR2(255) := NULL;
    v_debug      VARCHAR2(32);
    v_owner      VARCHAR2(30);
    v_table      VARCHAR2(30);
    --
  BEGIN
    v_owner     := GOOM.SplitOwnerObject(v_ownertable,'OWNER');
    v_table     := GOOM.SplitOwnerObject(v_ownertable,'TABLE');
    v_gfeatures := GOOM.GetGDOTableName('INGRFeatures');
    v_insert    := 'INSERT INTO '||v_gfeatures||'(FEATURENAME, GEOMETRYTYPE, PRIMARYGEOMETRYFIELDNAME, FEATUREDESCRIPTION ) select USER||''''.''''||''''';
    -- Collect metadata values
    v_query  := 'SELECT GEOMETRYTYPE, PRIMARYGEOMETRYFIELDNAME, FEATUREDESCRIPTION
                   FROM ' || v_gfeatures ||
                ' WHERE FEATURENAME=''' || v_ownertable || '''';
    i_cur := DBMS_SQL.OPEN_CURSOR;
    DBMS_SQL.PARSE( i_cur, v_query, DBMS_SQL.NATIVE);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 1, v_GeomType);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 2, v_PGeomField, 255);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 3, v_FeatDesc, 255);
    v_Dum := DBMS_SQL.EXECUTE(i_cur);
    LOOP
      IF (DBMS_SQL.FETCH_ROWS(i_cur) > 0) THEN
        DBMS_SQL.COLUMN_VALUE(i_cur, 1, v_GeomType);
        DBMS_SQL.COLUMN_VALUE(i_cur, 2, v_PGeomField);
        DBMS_SQL.COLUMN_VALUE(i_cur, 3, v_FeatDesc);
        v_FeatDesc := SUBSTR(REPLACE(v_FeatDesc, CHR(39), CHR(39) || CHR(39) || CHR(39) || CHR(39)), 1, 255);
        v_sql      := v_insert || v_table || c_sep || v_GeomType || c_sep || v_PGeomField || c_sep || v_FeatDesc || ''''' from dual';
        WRITE_BKP(v_gfeatures, v_table, v_sql, v_owner);
      ELSE
        EXIT;
      END IF;
    END LOOP;
    IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
       SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
        SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      GOOM.REPORT_ERROR(c_cmdname, v_query, v_debug, SQLCODE, SQLERRM);
  END BkpGFeatures;
  --
  --
  -- LibraryTables (only if schema is a GM Library or other derivitive)
  PROCEDURE BkpLibraryTable( v_ownertable IN VARCHAR2 ) IS
    c_cmdname    VARCHAR2(32)  := 'BkpLibraryTable';
    v_sql        VARCHAR2(2048):= NULL;
    v_TableType  VARCHAR2(64)  := NULL;
    v_owner      VARCHAR2(30);
    v_table      VARCHAR2(30);
    v_debug      VARCHAR2(32)  := 'INIT';
    --
  BEGIN
    v_owner := GOOM.SplitOwnerObject(v_ownertable,'OWNER');
    v_table := GOOM.SplitOwnerObject(v_ownertable,'TABLE');
    v_sql   := 'SELECT TABLETYPE FROM GDOSYS.LIBRARYTABLESBASE WHERE TABLENAME=:vownertable';
    EXECUTE IMMEDIATE v_sql INTO v_TableType USING v_ownertable;
    v_sql   := 'INSERT INTO GDOSYS.LIBRARYTABLESBASE(TABLETYPE,TABLENAME) select '''''|| v_TableType ||''''', USER||''''.''''||'''''|| v_table ||''''' FROM DUAL';
    WRITE_BKP('GDOSYS.LIBRARYTABLES', v_table, v_sql, v_owner);
    v_debug := 'Write Complete';
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_sql, v_debug, SQLCODE, SQLERRM);
  END BkpLibraryTable;
-- ----------------------------------------------------------------------------------------
  -- Procedure to backup the GDOSYS metadata assigned to a specific feature class
  -- v_table in the format USER.TABLE_NAME.  GDOSYS_BKP table must already exist.
  --
  PROCEDURE Bkp_TableMetadata( v_ownertable IN VARCHAR2) IS
    c_cmdname   CONSTANT VARCHAR2(17) := 'Bkp_TableMetadata';
    v_debug     VARCHAR2(32);
  BEGIN
    -- Backup FIELDLOOKUP, AttributeProperties, GeometryProperties
    IF GDOMetadataExists('INGRFieldLookup',v_ownertable) THEN
       BkpFieldLookup(v_ownertable);
       v_debug := 'INGRFieldLookup';
    END IF;
    -- Backup GFieldMapping
    IF GDOMetadataExists('GOracleFieldMapping',v_ownertable) THEN
       BkpGfieldMapping(v_ownertable);
       v_debug := 'GOracleFieldMapping';
    END IF;
    -- Backup GINDEX_COLUMNS
    IF GDOMetadataExists('GOracleIndexColumns',v_ownertable) THEN
       BkpGIndexColumns(v_ownertable);
       v_debug := 'GOracleIndexColumns';
    END IF;
    -- Backup GPickLists if it exists.
    IF GDOMetadataExists('INGRPickLists',v_ownertable) THEN
       BkpGPickList(v_ownertable);
       v_debug := 'INGRPickLists';
    END IF;
    -- Backup Library Metadata if it exists
    IF GDOMetadataExists('LibraryTables',v_ownertable) THEN
       BkpLibraryTable(v_ownertable);
       v_debug := 'LibraryTables';
    END IF;
    -- Backup GFeatures - Must be done last.
    IF GDOMetadataExists('INGRFeatures',v_ownertable) THEN
       BkpGFeatures(v_ownertable);
       v_debug := 'INGRFeatures';
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_ownertable, 'GDO:'||v_debug, SQLCODE, SQLERRM);
  END Bkp_TableMetadata;
  --
  -- Procedure to backup entries in Oracle's USER_SDO_GEOM_METADATA view
  PROCEDURE Bkp_OracleMetadata( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'Bkp_OracleMetadata';
    c_msg           CONSTANT VARCHAR2(32) := 'Oracle Metadata saved in: ';
    --
    v_sql           VARCHAR2(2048);
    v_debug         VARCHAR2(255);
  BEGIN
    IF GOOM.chkTable(v_owner||'.BKP_SDO_GEOM_METADATA') THEN
      EXECUTE IMMEDIATE 'DROP TABLE '||v_owner||'.BKP_SDO_GEOM_METADATA PURGE';
      v_debug := 'Dropped existing BKP_SDO_GEOM_METADATA';
    END IF;
    -- Direct transfer from Oracle metadata view to local table.
    v_sql := 'CREATE TABLE '||v_owner||'.BKP_SDO_GEOM_METADATA as SELECT TABLE_NAME,COLUMN_NAME,DIMINFO,SRID FROM ALL_SDO_GEOM_METADATA WHERE OWNER ='''||v_owner||'''';
    EXECUTE IMMEDIATE v_sql;
    v_debug := 'Created BKP_SDO_GEOM_METADATA Table';
    BkpResponse(c_cmdname,c_msg||v_owner||'.BKP_SDO_GEOM_METADATA');
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_sql, v_debug, SQLCODE, SQLERRM);
  END Bkp_OracleMetadata;
-- -------------------------------------------------------------------------
  -- Backup GCOORDSYSTEM and GPARAMETERS
  --
  PROCEDURE SaveGCoordSystem( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'SaveGCoordSystem';
    c_CSmsg         CONSTANT VARCHAR2(32) := 'Coordinate system(s) saved in: ';
    c_DefCSmsg      CONSTANT VARCHAR2(37) := 'Default coordinate system saved in: ';
    --
    v_gcoordtable   VARCHAR2(64);
    v_gfieldtable   VARCHAR2(64);
    v_gparmtable    VARCHAR2(64);
    v_sql           VARCHAR2(2048) := NULL;
    v_csguid        VARCHAR2(64)   := NULL;
    v_count         PLS_INTEGER    := 0;
    v_debug         VARCHAR2(255);
  BEGIN
    IF GOOM.chkTable(v_owner||'.GCOORDSYS_BKP') THEN
      EXECUTE IMMEDIATE 'DROP TABLE '||v_owner||'.GCOORDSYS_BKP PURGE';
      v_debug := 'Dropped existing GCOORDSYS_BKP';
    END IF;
    IF GOOM.chkTable(v_owner||'.GCOORDSYS_DEF') THEN
      EXECUTE IMMEDIATE 'DROP TABLE '||v_owner||'.GCOORDSYS_DEF PURGE';
      v_debug := 'Dropped existing GCOORDSYS_DEF';
    END IF;
    -- Get table information then create backup tables.
    v_gcoordtable := GOOM.GetGDOTableName('GCoordSystemTable');
    v_gfieldtable := GOOM.GetGDOTableName('GOracleFieldMapping');
    v_gparmtable  := GOOM.GetGDOTableName('GParameters');
    v_sql := 'CREATE TABLE '||v_owner||'.GCOORDSYS_BKP as SELECT * FROM '||v_gcoordtable||' WHERE CSGUID IN (SELECT CSGUID from '||v_gfieldtable||' where OWNER='''||v_owner||''')';
    EXECUTE IMMEDIATE v_sql;
    v_debug := 'Created GCOORDSYS_BKP Table';
    BkpResponse(c_cmdname,c_CSmsg||v_owner||'.GCOORDSYS_BKP');
    -- Check GPARAMETERS for a default coordinate system and add it to GCOORDSYS_DEF table.
    v_sql := 'SELECT count(1) FROM ' || v_gparmtable || ' WHERE SUBSTR(GPARAMETER,1,INSTR(GPARAMETER,''.'')-1)=''' || v_owner || '''';
    EXECUTE IMMEDIATE v_sql INTO v_count;
    IF v_count > 0 THEN
      v_debug := 'Checking GPARAMETERS for Default CS';
      v_sql:='SELECT GVALUE FROM ' || v_gparmtable || ' WHERE SUBSTR(GPARAMETER,1,INSTR(GPARAMETER,''.'')-1)=''' || v_owner || '''';
      EXECUTE IMMEDIATE v_sql INTO v_csguid;
      v_debug := 'Creating GCOORDSYS_DEF table.';
      EXECUTE IMMEDIATE 'CREATE TABLE '||v_owner||'.GCOORDSYS_DEF (CSGUID VARCHAR2(64) PRIMARY KEY)';
      v_debug := 'Populating GCOORDSYS_DEF table.';
      EXECUTE IMMEDIATE 'INSERT INTO '||v_owner||'.GCOORDSYS_DEF VALUES(''' || v_csguid || ''')';
      v_debug := 'Default CS Saved in GCOORDSYS_DEF';
      BkpResponse(c_cmdname,c_DefCSmsg||v_owner||'.GCOORDSYS_DEF');
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_sql, v_debug, SQLCODE, SQLERRM);
  END SaveGCoordSystem;
  -- --------------------------------------------------------------------------
  -- Restore GDOSYS metadata from GDOSYS_BKP
  --
  PROCEDURE RestoreTableMetadata( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname   CONSTANT VARCHAR2(20) := 'RestoreTableMetadata';
    c_feedback1 CONSTANT VARCHAR2(40) := 'Inserted metadata for tables in schema: ';
    --
    v_restore   VARCHAR2(4096);
    v_fc        VARCHAR2(32);
    v_sql       VARCHAR2(4096);
    i_cur       PLS_INTEGER;
    v_dum       PLS_INTEGER;
  BEGIN
    v_sql := 'SELECT FEATURECLASS, INSERT_SQL FROM '||v_owner||'.GDOSYS_BKP ORDER BY PID';
    i_cur := DBMS_SQL.OPEN_CURSOR;
    DBMS_SQL.PARSE( i_cur, v_sql, DBMS_SQL.NATIVE);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 1, v_fc, 32);
    DBMS_SQL.DEFINE_COLUMN(i_cur, 2, v_restore, 2048);
    v_dum := DBMS_SQL.EXECUTE(i_cur);
    LOOP
      IF (DBMS_SQL.FETCH_ROWS(i_cur) = 0) THEN
        EXIT;
      END IF;
      DBMS_SQL.COLUMN_VALUE(i_cur, 1, v_fc);
      DBMS_SQL.COLUMN_VALUE(i_cur, 2, v_restore);
      EXECUTE IMMEDIATE v_restore;
      COMMIT;
    END LOOP;
    IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
       SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    END IF;
    BkpResponse(c_cmdname, c_feedback1 || v_owner);
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      GOOM.REPORT_ERROR(c_cmdname, v_fc, v_sql, v_restore, SQLERRM);
  END RestoreTableMetadata;
  -- -----------------------------------------------------------------------------
  -- Restore Oracle metadata if it was saved.
  --
  PROCEDURE RestoreOracleMetadata( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'RestoreOracleMetadata';
    c_emsg          CONSTANT VARCHAR2(48) := 'Oracle metadata is missing and was not restored.';
    c_smsg          CONSTANT VARCHAR2(55) := 'Oracle metadata was restored to USER_SDO_GEOM_METADATA.';
    --
    v_sql           VARCHAR2(2048);
    v_debug         VARCHAR2(255);
    v_dum           PLS_INTEGER;
  BEGIN
    IF NOT GOOM.chkTable('BKP_SDO_GEOM_METADATA') THEN
      RAISE e_TableNotFound;
    END IF;
    IF v_owner = USER THEN
      v_sql := 'DELETE FROM USER_SDO_GEOM_METADATA';
      EXECUTE IMMEDIATE v_sql;
      COMMIT;
      v_sql := 'INSERT INTO USER_SDO_GEOM_METADATA SELECT TABLE_NAME,COLUMN_NAME,DIMINFO,SRID FROM BKP_SDO_GEOM_METADATA';
      EXECUTE IMMEDIATE v_sql;
      COMMIT;
      BkpResponse(c_cmdname,c_smsg);
    ELSE
      v_sql := 'SELECT DISTINCT(1) FROM MDSYS.SDO_GEOM_METADATA_TABLE';
      EXECUTE IMMEDIATE v_sql into v_dum;
      v_sql := 'DELETE FROM MDSYS.SDO_GEOM_METADATA_TABLE WHERE SDO_OWNER=:vowner';
      EXECUTE IMMEDIATE v_sql using v_owner;
      COMMIT;
      v_sql := 'INSERT INTO MDSYS.SDO_GEOM_METADATA_TABLE SELECT :vowner,TABLE_NAME,COLUMN_NAME,DIMINFO,SRID FROM BKP_SDO_GEOM_METADATA';
      EXECUTE IMMEDIATE v_sql using v_owner;
      COMMIT;
      BkpResponse(c_cmdname,c_smsg);
    END IF;
  EXCEPTION
    WHEN e_NoPrivileges THEN
       BkpResponse(c_msgWarning||c_cmdname,c_msgNoPrivileges);
    WHEN e_TableNotFound THEN
       BkpResponse(c_cmdname,c_emsg);
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_sql, v_debug, SQLCODE, SQLERRM);
  END RestoreOracleMetadata;
  -- -----------------------------------------------------------------------------
  -- Restore the coordinate system values from the local backup table.
  --
  PROCEDURE RestoreGCoordSystem( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname           CONSTANT VARCHAR2(19) := 'RestoreGCoordSystem';
    c_csRestored        CONSTANT VARCHAR2(35) := 'Coordinate System Metadata Restored';
    c_csAlreadyExists   CONSTANT VARCHAR2(44) := 'GDOSYS.GCOORDSYSTEM.CSGUID already exists: ';
    c_csTabNotExist     CONSTANT VARCHAR2(47) := 'The GCOORDSYS_BKP backup table does not exist.';
    c_DefCSExists       CONSTANT VARCHAR2(69) := 'Default Coordinate System already exists in GDOSYS.GPARAMETERS for: ';
    c_DefCSRestored     CONSTANT VARCHAR2(41) := 'Default Coordinate System Restored for: ';
    --
    v_sql               VARCHAR2(2048)  := NULL;
    v_query             VARCHAR2(2048)  := NULL;
    v_debug             VARCHAR2(255)   := NULL;
    v_csguid            VARCHAR2(64)    := NULL;
    v_defcsguid         VARCHAR2(64)    := NULL;
    v_count             PLS_INTEGER     := 0;
    i_cur               PLS_INTEGER;
    v_dum               PLS_INTEGER;
  BEGIN
    IF GOOM.chkTable(v_owner||'.GCOORDSYS_BKP') THEN
      v_query := 'SELECT CSGUID FROM '||v_owner||'.GCOORDSYS_BKP';
      i_cur := DBMS_SQL.OPEN_CURSOR;
      DBMS_SQL.PARSE( i_cur, v_query, DBMS_SQL.NATIVE);
      DBMS_SQL.DEFINE_COLUMN(i_cur, 1, v_csguid, 64);
      v_dum := DBMS_SQL.EXECUTE(i_cur);
      LOOP
        IF (DBMS_SQL.FETCH_ROWS(i_cur) = 0) THEN
          EXIT;
        END IF;
        DBMS_SQL.COLUMN_VALUE(i_cur, 1, v_csguid);
        SELECT COUNT(1) INTO v_count FROM GDOSYS.GCOORDSYSTEM WHERE CSGUID = v_csguid;
        v_debug := 'CSGUID COUNT:' || v_count;
        IF v_count < 1 THEN
          v_sql := 'INSERT INTO GDOSYS.GCOORDSYSTEM SELECT * FROM '||v_owner||'.GCOORDSYS_BKP WHERE CSGUID=''' || v_csguid || '''';
          EXECUTE IMMEDIATE v_sql;
          COMMIT;
          BkpResponse(c_cmdname , c_csRestored);
        ELSE
          BkpResponse(c_msgWarning||c_cmdname, c_csAlreadyExists || v_csguid);
        END IF;
      END LOOP;
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
    ELSE
      BkpResponse(c_msgWarning||c_cmdname,c_csTabNotExist);
    END IF;
    IF GOOM.chkTable(v_owner||'.GCOORDSYS_DEF') THEN
      v_debug := 'Processing Default CS - Table exists';
      v_sql   := 'SELECT COUNT(1) FROM '||v_owner||'.GCOORDSYS_DEF';
      EXECUTE IMMEDIATE v_sql INTO v_count;
      IF v_count = 0 THEN
        RAISE e_NoDataFound;
      END IF;
      v_sql   := 'SELECT CSGUID FROM '||v_owner||'.GCOORDSYS_DEF';
      EXECUTE IMMEDIATE v_sql INTO v_defcsguid;
      SELECT COUNT(1) INTO v_count FROM GDOSYS.GPARAMETERS WHERE GPARAMETER = v_owner || '.DefaultCoordinateSystem';
      v_debug := 'Default CSGUID COUNT:' || v_count;
      IF v_count < 1 THEN
        v_sql := 'INSERT INTO GDOSYS.GPARAMETERS SELECT ' || '''' || v_owner || '.DefaultCoordinateSystem'',''' || v_defcsguid || ''' from dual';
        EXECUTE IMMEDIATE v_sql;
        COMMIT;
        BkpResponse(c_cmdname, c_DefCSRestored || v_owner);
      ELSE
        BkpResponse(c_msgWarning||c_cmdname, c_DefCSExists || v_owner);
      END IF;
    END IF;
  EXCEPTION
    WHEN e_NoDataFound THEN
      BkpResponse(c_msgWarning||c_cmdname,c_msgNoDataFound||v_owner||'.GCOORDSYS_DEF');
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
          SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      GOOM.REPORT_ERROR(c_cmdname, v_sql, v_debug, SQLCODE, SQLERRM);
  END RestoreGCoordSystem;
  -- ----------------------------------------------------------------------------------------
  -- Backup Verification Procedure
  --
  PROCEDURE VerifyGDOSYSBkp( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(15) := 'VerifyGDOSYSBkp';
    c_err1          CONSTANT VARCHAR2(53) := 'No Backup: Run EXEC GDOBKP.SaveGDOSYSMetadata; first.';
    c_feedback      CONSTANT VARCHAR2(44) := 'Review and determine cause of any failures.';
    c_csmsg1        CONSTANT VARCHAR2(35) := 'No coordinate systems to restore.';
    c_csmsg2        CONSTANT VARCHAR2(40) := 'No Default coordinate system to restore.';
    --
    v_fieldlookup   VARCHAR2(61);
    v_gfieldmapping VARCHAR2(61);
    v_geometryprop  VARCHAR2(61);
    v_attributeprop VARCHAR2(61);
    v_gfeatures     VARCHAR2(61);
    v_gcoordsys     VARCHAR2(61);
    v_gparameters   VARCHAR2(61);
    v_picklists     VARCHAR2(61);
    v_indexcols     VARCHAR2(61);
    v_glibrarytab   VARCHAR2(61);
    v_INCount       PLS_INTEGER;
    v_OUTCount      PLS_INTEGER;
    v_sql           VARCHAR2(4096);
  BEGIN
    GOOM.DashLine;
    BkpResponse(c_cmdname,c_msgStart||v_owner,40);
    IF NOT GOOM.chkTable(v_owner||'.GDOSYS_BKP') THEN
      -- If the backup does not exist, exit.
      BkpResponse(c_cmdname,c_err1);
      GOTO VerifyEnd;
    END IF;
    -- Collect metadata table names.
    v_fieldlookup   := GOOM.GetGDOTablename('INGRFieldLookup');
    v_gfieldmapping := GOOM.GetGDOTablename('GOracleFieldMapping');
    v_attributeprop := GOOM.GetGDOTablename('INGRAttributeProperties');
    v_geometryprop  := GOOM.GetGDOTablename('INGRGeometryProperties');
    v_gfeatures     := GOOM.GetGDOTablename('INGRFeatures');
    v_picklists     := GOOM.GetGDOTablename('INGRPickLists');
    v_indexcols     := GOOM.GetGDOTablename('GOracleIndexColumns');
    v_gcoordsys     := GOOM.GetGDOTablename('GCoordSystemTable');
    v_gparameters   := GOOM.GetGDOTablename('GParameters');
    v_glibrarytab   := GOOM.GetGDOTablename('LibraryTables');
    -- Verify the counts.
    v_sql := 'SELECT COUNT(*) FROM ' || v_fieldlookup || ' WHERE FEATURENAME like '''||v_owner||'.%''';
    EXECUTE IMMEDIATE v_sql INTO v_INCount;
    v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.GDOSYS_BKP WHERE GDOSYS_TABLE=''' || v_fieldlookup || '''';
    EXECUTE IMMEDIATE v_sql INTO v_OUTCount;
    VerifyCount(v_INCount, v_OUTCount, v_fieldlookup);
    --
    v_sql := 'SELECT COUNT(*) FROM ' || v_attributeprop || ' WHERE INDEXID IN (SELECT INDEXID FROM ' || v_fieldlookup ||
             ' WHERE FEATURENAME LIKE '''||v_owner||'.%'')';
    EXECUTE IMMEDIATE v_sql INTO v_INCount;
    v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.GDOSYS_BKP WHERE GDOSYS_TABLE=''' || v_attributeprop || '''';
    EXECUTE IMMEDIATE v_sql INTO v_OUTCount;
    VerifyCount(v_INCount, v_OUTCount, v_attributeprop);
    --
    v_sql := 'SELECT COUNT(*)FROM ' || v_geometryprop || ' WHERE INDEXID IN (SELECT INDEXID FROM ' || v_fieldlookup ||
             ' WHERE FEATURENAME LIKE '''||v_owner||'.%'')';
    EXECUTE IMMEDIATE v_sql INTO v_INCount;
    v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.GDOSYS_BKP WHERE GDOSYS_TABLE=''' || v_geometryprop || '''';
    EXECUTE IMMEDIATE v_sql INTO v_OUTCount;
    VerifyCount(v_INCount, v_OUTCount, v_geometryprop);
    --
    v_sql := 'SELECT COUNT(*) FROM ' || v_gfieldmapping || ' WHERE OWNER='''||v_owner||'''';
    EXECUTE IMMEDIATE v_sql INTO v_INCount;
    v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.GDOSYS_BKP WHERE GDOSYS_TABLE=''' || v_gfieldmapping || '''';
    EXECUTE IMMEDIATE v_sql INTO v_OUTCount;
    VerifyCount(v_INCount, v_OUTCount, v_gfieldmapping);
    --
    v_sql := 'SELECT COUNT(*) FROM ' || v_gfeatures || ' WHERE FEATURENAME LIKE '''||v_owner||'.%''';
    EXECUTE IMMEDIATE v_sql INTO v_INCount;
    v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.GDOSYS_BKP WHERE GDOSYS_TABLE=''' || v_gfeatures || '''';
    EXECUTE IMMEDIATE v_sql INTO v_OUTCount;
    VerifyCount(v_INCount, v_OUTCount, v_gfeatures);
    --
    IF v_picklists IS NOT NULL THEN
      v_sql := 'SELECT COUNT(*) FROM ' || v_picklists || ' WHERE FEATURENAME LIKE '''||v_owner||'.%''';
      EXECUTE IMMEDIATE v_sql INTO v_INCount;
      v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.GDOSYS_BKP WHERE GDOSYS_TABLE=''' || v_picklists || '''';
      EXECUTE IMMEDIATE v_sql INTO v_OUTCount;
      VerifyCount(v_INCount, v_OUTCount, v_picklists);
    END IF;
    --
    v_sql := 'SELECT COUNT(*) FROM ' || v_indexcols || ' WHERE OWNER = '''||v_owner||'''';
    EXECUTE IMMEDIATE v_sql INTO v_INCount;
    v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.GDOSYS_BKP WHERE GDOSYS_TABLE=''' || v_indexcols || '''';
    EXECUTE IMMEDIATE v_sql INTO v_OUTCount;
    VerifyCount(v_INCount, v_OUTCount, v_indexcols);
    --
    v_sql := 'SELECT COUNT(*) FROM ' || v_glibrarytab || ' WHERE SUBSTR(TABLENAME,1,INSTR(TABLENAME,''.'',1)-1)= '''||v_owner||'''';
    EXECUTE IMMEDIATE v_sql INTO v_INCount;
    v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.GDOSYS_BKP WHERE GDOSYS_TABLE=''' || v_glibrarytab || '''';
    EXECUTE IMMEDIATE v_sql INTO v_OUTCount;
    VerifyCount(v_INCount, v_OUTCount, v_glibrarytab);
    --
    <<VerifyEnd>>
  --
    IF GOOM.chkTable(v_owner||'.GCOORDSYS_BKP') THEN
      v_sql := 'SELECT COUNT(*) FROM ' || v_gcoordsys || ' WHERE CSGUID IN (SELECT DISTINCT(GCOORDSYSTEMGUID) GUID FROM ' || v_geometryprop ||
               ' WHERE INDEXID IN (SELECT INDEXID FROM ' || v_fieldlookup || ' WHERE FEATURENAME like '''||v_owner||'.%''))';
      EXECUTE IMMEDIATE v_sql INTO v_INCount;
      v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.GCOORDSYS_BKP';
      EXECUTE IMMEDIATE v_sql INTO v_OUTCount;
      VerifyCount(v_INCount, v_OUTCount, v_gcoordsys);
    ELSE
      BkpResponse(c_msgWarning,c_csmsg1);
    END IF;
    --
    IF GOOM.chkTable(v_owner||'.GCOORDSYS_DEF') THEN
      v_sql := 'SELECT COUNT(*) FROM ' || v_gparameters || ' WHERE GPARAMETER LIKE '''||v_owner||'.%''';
      EXECUTE IMMEDIATE v_sql INTO v_INCount;
      v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.GCOORDSYS_DEF';
      EXECUTE IMMEDIATE v_sql INTO v_OUTCount;
      VerifyCount(v_INCount, v_OUTCount, v_gparameters);
    ELSE
      BkpResponse(c_msgWarning,c_csmsg2);
    END IF;
    --
    IF GOOM.chkTable(v_owner||'.BKP_SDO_GEOM_METADATA') THEN
      v_sql := 'SELECT COUNT(*) FROM '||v_owner||'.BKP_SDO_GEOM_METADATA';
      EXECUTE IMMEDIATE v_sql INTO v_INCount;
      v_sql := 'SELECT COUNT(*) FROM ALL_SDO_GEOM_METADATA WHERE OWNER=:vowner';
      EXECUTE IMMEDIATE v_sql INTO v_OUTCount USING v_owner;
      VerifyCount(v_INCount, v_OUTCount, 'USER_SDO_GEOM_METADATA');
    END IF;
    --
    BkpResponse(c_cmdname,c_msgComplete||v_owner,40);
    BkpResponse('NOTE',c_feedback,40);
    Goom.DashLine;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_sql, 'None', SQLCODE, SQLERRM);
  END VerifyGDOSYSBkp;
  -- --------------------------------------------------------------------------------
  -- These are the main procedures used to backup and restore GDOSYS metadata.
  -- SaveGDOSYSMetadata can be run by an Administrator if the schema owner is specified.
  -- RestoreGDOSYSMetadata can ONLY be run by the schema that owns the tables being processed.
  --
  PROCEDURE SaveGDOSYSMetadata( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(20) := 'SaveGDOSYSMetadata';
    v_object        GetSchemaObjects%ROWTYPE;
    v_ownerobject   VARCHAR2(64);
  BEGIN
    GOOM.DelGDOSYSOrphans(v_owner);                                                 -- Clear orphans for this user
    GOOM.DeleteOrphanMBR(v_owner);                                                  -- Clear MBR Orphans for this user
    Create_GDOSYS_BKP_Table(v_owner);                                               -- Create the backup metadata table
    GOOM.DashLine;                                                                  -- Formatting
    BkpResponse(c_cmdname,c_msgStart||v_owner,40);                                  -- Send start msg to user
    FOR v_object IN GetSchemaObjects(v_owner) LOOP                                  -- Loop through all schema objects
      v_ownerobject := v_owner||'.'||v_object.object_name;                          -- Collect owner.object
      Bkp_TableMetadata(v_ownerobject);                                             -- Backup Main Metadata Tables
      BkpResponse(v_ownerobject,c_msgMetadataSaved||v_owner||'.GDOSYS_BKP');        -- Notify User of FC Status
    END LOOP;                                                                       -- Done with all objects
    SaveGCoordSystem(v_owner);                                                      -- Backup Coordinate System and GPARAMETERS
    Bkp_OracleMetadata(v_owner);                                                    -- Backup Oracle's spatial metadata.
    BkpResponse(c_cmdname,c_msgComplete||v_owner,40);                               -- Send stop msg to user
    GOOM.DashLine;                                                                  -- Formatting
    VerifyGDOSYSBkp(v_owner);                                                       -- Verify Backup
  EXCEPTION
    WHEN e_NoGDOSYSMetadata THEN
      BkpResponse(c_cmdname || c_msgWarning, c_msgNoGDOSYSMetadata || v_owner);
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_ownerobject, 'None', SQLCODE, SQLERRM);
  END SaveGDOSYSMetadata;
  --
  PROCEDURE RestoreGDOSYSMetadata( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(21) := 'RestoreGDOSYSMetadata';
  BEGIN
    IF v_owner != USER THEN
      RAISE e_TabOwnerError;
    END IF;
    -- Start process
    GOOM.DashLine;
    BkpResponse(c_cmdname,c_msgStart||v_owner,40);
    -- Actual work is done here calling local procedures
    DeleteGDOSYSMetadata(v_owner, FALSE);       -- Clear existing metadata
    RestoreTableMetadata(v_owner);              -- Restore Tablemetadata
    RestoreGCoordSystem(v_owner);               -- Restore coordinate systems
    RestoreOracleMetadata(v_owner);             -- Restore Oracle metadata
    -- Process is done, notify the user.
    BkpResponse(c_cmdname,c_msgComplete||v_owner,40);
    GOOM.DashLine;
    -- Process complete, now verify
    VerifyGDOSYSBkp(v_owner);
    VerifySequenceOwner(v_owner);
    VerifyLibraryObjectTypeRef;
  EXCEPTION
    WHEN e_TabOwnerError THEN
      GOOM.Response(c_msgerror||c_cmdname,c_msgTabOwnerError || v_owner ||'<>'||USER);
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_owner, v_owner, SQLCODE, SQLERRM);
  END RestoreGDOSYSMetadata;
  --
  -- -------------------------------------------------------------------------------
  -- This procedure will delete all the GDOSYS metadata for a schema.
  --
  PROCEDURE DeleteGDOSYSMetadata( v_owner IN VARCHAR2 DEFAULT USER, v_respn IN BOOLEAN DEFAULT TRUE) IS
    c_cmdname   VARCHAR2(32) := 'DeleteGDOSYSMetadata';
    v_feature   ALL_OBJECTS%ROWTYPE;
  BEGIN
    FOR v_feature IN GetSchemaObjects (v_owner) LOOP
      GOOM.DelGDOSYSMetadata(v_owner||'.'||v_feature.object_name, v_respn);
    END LOOP;
    GOOM.DeleteOrphanCS(v_respn);
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_feature.object_name, v_owner, SQLCODE, SQLERRM);
  END DeleteGDOSYSMetadata;
  --
  -- -------------------------------------------------------------------------------
  -- This procedure will delete all the metadata backup tables from the schema.
  --
  PROCEDURE DeleteBackupTables( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname VARCHAR2(32) := 'DeleteBackupTables';
  BEGIN
    IF GOOM.chkTable(v_owner||'.GDOSYS_BKP') THEN
      EXECUTE IMMEDIATE 'DROP TABLE '||v_owner||'.GDOSYS_BKP PURGE';
      EXECUTE IMMEDIATE 'DROP SEQUENCE '||v_owner||'.GDOSYS_BKP_SEQ';
      GOOM.Response('DELETED TABLE',v_owner||'.GDOSYS_BKP');
    ELSE
      GOOM.Response('WARNING',v_owner||'.GDOSYS_BKP does not exist');
    END IF;
    IF GOOM.chkTable(v_owner||'.GCOORDSYS_BKP') THEN
      EXECUTE IMMEDIATE 'DROP TABLE '||v_owner||'.GCOORDSYS_BKP PURGE';
      GOOM.Response('DELETED TABLE',v_owner||'.GCOORDSYS_BKP');
    ELSE
      GOOM.Response('WARNING',v_owner||'.GCOORDSYS_BKP does not exist');
    END IF;
    IF GOOM.chkTable(v_owner||'.GCOORDSYS_DEF') THEN
      EXECUTE IMMEDIATE 'DROP TABLE '||v_owner||'.GCOORDSYS_DEF PURGE';
      GOOM.Response('DELETED TABLE',v_owner||'.GCOORDSYS_DEF');
    ELSE
      GOOM.Response('WARNING',v_owner||'.GCOORDSYS_DEF does not exist');
    END IF;
    IF GOOM.chkTable(v_owner||'.BKP_SDO_GEOM_METADATA') THEN
      EXECUTE IMMEDIATE 'DROP TABLE '||v_owner||'.BKP_SDO_GEOM_METADATA PURGE';
      GOOM.Response('DELETED TABLE',v_owner||'.BKP_SDO_GEOM_METADATA');
    ELSE
      GOOM.Response('WARNING',v_owner||'.BKP_SDO_GEOM_METADATA does not exist');
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR(c_cmdname, v_owner, 'None', SQLCODE, SQLERRM);
  END DeleteBackupTables;
  -- -------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
-- This ends the package functions and procedures.
END GDOBKP;
/
---------------------------------------------------------------------------------------
-- Grant privileges and create PUBLIC synonym for package.
SET SERVEROUTPUT ON;
SHOW ERRORS
PROMPT **..............................................................**;
PROMPT ** NOTE: Any errors occurring above may leave package unusable. **;
SET TERMOUT OFF
GRANT EXECUTE ON GDOSYS.GDOBKP TO PUBLIC;
SET TERMOUT ON
PROMPT **       Execute Privilege on GDOBKP Package Granted to PUBLIC. **;
SET TERMOUT OFF
DROP PUBLIC SYNONYM GDOBKP;
CREATE PUBLIC SYNONYM GDOBKP FOR GDOSYS.GDOBKP;
SET TERMOUT ON
PROMPT **       PUBLIC Synonym GDOBKP has been created.                **;
PROMPT **..............................................................**;
PROMPT ** To turn on package messages, run:     SET SERVEROUTPUT ON    **;  
PROMPT ** For version information, run:         EXEC GDOBKP.VERSION    **;
PROMPT ** For Online Help, run:                 EXEC GDOBKP.HELPME     **;
PROMPT ** ***************    Installation completed.   *************** **;
PROMPT ******************************************************************;
EXEC GDOBKP.VERSION;
---------------------------------------------------------------------------------------
