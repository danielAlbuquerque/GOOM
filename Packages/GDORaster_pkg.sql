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
-- GDORASTER_pkg.sql
-- GeoMedia Raster Utilities (GDORASTER) Package
-- Oracle Object Model Procedures and Functions for use with GeoMedia's GeoRaster.
--
-- Before installing this package, the following is REQUIRED:
-- - The database must be Oracle 11g or later
-- - The GDOSYS metadata schema must already exist.
-- - The GOOM package must be installed first.
-- - The package must be installed by a DBA user.
--
-- Any user account that makes use of this package must be explicitly granted the 
-- following privileges:
-- GRANT CREATE TABLE TO <user>;
-- GRANT CREATE SEQUENCE TO <user>;
-- These privileges are required by Oracle when manipulating spatial (domain based) indexes.
-- ----------------------------------------------------------------------------------------
--
-- Release History:
-- 03/21/2008  Released to WEB.
-- 04/10/2009  Released to WEB.
-- 04/30/2010  Released to WEB.
-- 03/15/2012  Released to WEB.
-- 07/15/2013  Released to WEB.
-- 07/15/2014  Released to WEB.
-- 07/15/2015  Released to WEB.
-- 07/15/2016  Released to WEB.
--
-- History since previous release: 
-- 03/25/2016  Package is frozen, no more enhancements.
-- 
-- ----------------------------------------------------------------------------------------
PROMPT ******************************************************************;
PROMPT **                  GDORaster PACKAGE 2016                      **;
PROMPT **                        Intergraph                            **;
PROMPT **                                                              **;
PROMPT **   This package provides various utility functions and        **;
PROMPT **   procedures for use with Oracle GeoRaster and GeoMedia.     **;
PROMPT **..............................................................**;
PROMPT **              IMPORTANT NOTES: PLEASE READ THIS               **;
PROMPT ** -> Installation requires a DBA connection.                   **;
PROMPT ** -> The GOOM package is a required pre-requsite.              **;
PROMPT ** -> NOTE: Georaster requires Oracle Spatial 11G.              **;
PROMPT ** -> Package is installed into the GDOSYS schema by default.   **;
PROMPT ** -> Package uses a RASTERS tablespace. DBA can change this.   **;
PROMPT ** -> This is an unofficial package, email support only.        **;
PROMPT ** -> This package uses a global temporary table called         **;
PROMPT **    GDOSYS.GDORASTER_STATS_TMP.  DO NOT DELETE this table.    **;
PROMPT **..............................................................**;
-- ----------------------------------------------------------------------------------------
-- Create the GDORASTER_STATS_TMP table in GDOSYS.  This stores stats for all RDT images.
-- 
DECLARE
  v_sql      VARCHAR2(512);
  v_count    PLS_INTEGER;
BEGIN
  SELECT COUNT(1) INTO v_count FROM ALL_TABLES where OWNER='GDOSYS' and TABLE_NAME='GDORASTER_STATS_TMP';
  IF v_count != 0 THEN
    EXECUTE IMMEDIATE 'DROP TABLE GDOSYS.GDORASTER_STATS_TMP';
  END IF;
  v_sql:='CREATE GLOBAL TEMPORARY TABLE GDOSYS.GDORASTER_STATS_TMP(VALUE NUMBER) ON COMMIT DELETE ROWS';
  EXECUTE IMMEDIATE v_sql;
  v_sql:='GRANT ALL ON GDOSYS.GDORASTER_STATS_TMP TO PUBLIC';
  EXECUTE IMMEDIATE v_sql;
END;
/
-- ----------------------------------------------------------------------------------------
CREATE OR REPLACE PACKAGE GDOSYS.GDORASTER AUTHID CURRENT_USER IS
-- ----------------------------------------------------------------------------------------
-- Misc Procedures
PROCEDURE VERSION;
PROCEDURE HELPME( v_topic IN VARCHAR2 DEFAULT NULL);
-- 
PROCEDURE DropRasterTable( v_tablename IN VARCHAR2);
PROCEDURE CreateRasterTable( v_tablename IN VARCHAR2);
PROCEDURE CreatePCIRasterTable( v_tablename IN VARCHAR2);
PROCEDURE CreateRasterIndex( v_tablename IN VARCHAR2 DEFAULT NULL);
PROCEDURE RasterIndexAll( v_owner IN VARCHAR2 DEFAULT USER);
--
PROCEDURE LoadRasterImage( v_tablename IN VARCHAR2, 
                            v_filename IN VARCHAR2, 
                           v_worldfile IN VARCHAR2, 
                                i_srid IN INTEGER, 
                                v_desc IN VARCHAR2 DEFAULT NULL,
                             v_blksize IN VARCHAR2 DEFAULT NULL,
                            v_compress IN VARCHAR2 DEFAULT 'NONE',
                             i_quality IN INTEGER  DEFAULT NULL,
                              v_update IN VARCHAR2 DEFAULT 'Y');
--
PROCEDURE UpdateImageAttributes( v_tablename IN VARCHAR2, 
                                   i_imageid IN INTEGER);
--
PROCEDURE SetRasterExtents ( v_tablename IN VARCHAR2, 
                                   v_id IN INTEGER DEFAULT NULL);
--
PROCEDURE SetDefaultRasterMBR( v_tablename IN VARCHAR2, 
                                     v_typ IN VARCHAR2 DEFAULT 'P');
--
PROCEDURE AlterImageParameters( v_tablename IN VARCHAR2, 
                                  i_imageid IN INTEGER, 
                                 v_blocking IN VARCHAR2, 
                               v_interleave IN VARCHAR2 DEFAULT NULL,
                              v_compression IN VARCHAR2 DEFAULT 'NONE',
                                  i_quality IN INTEGER  DEFAULT NULL,
                                   v_update IN VARCHAR2 DEFAULT 'Y');
--
PROCEDURE CopyImageWithParameters( v_tablename IN VARCHAR2, 
                                     i_imageid IN INTEGER, 
                                    v_blocking IN VARCHAR2, 
                                  v_interleave IN VARCHAR2 DEFAULT NULL,
                                 v_compression IN VARCHAR2 DEFAULT 'NONE',
                                     i_quality IN INTEGER DEFAULT NULL,
                                      v_update IN VARCHAR2 DEFAULT 'Y');
--
PROCEDURE CopyImage( v_tablename IN VARCHAR2, 
                       i_imageid IN INTEGER);
--
PROCEDURE SetImagePyramid( v_tablename IN VARCHAR2,
                             i_imageid IN INTEGER, 
                               i_level IN INTEGER DEFAULT NULL,
                              v_method IN VARCHAR2 DEFAULT 'AVERAGE4',
                              v_update IN VARCHAR2 DEFAULT 'Y');
--
PROCEDURE SetImageStatistics( v_tablename IN VARCHAR2, 
                                i_imageid IN INTEGER,
                                    n_min IN NUMBER,
                                    n_max IN NUMBER,
                                   n_mean IN NUMBER,
                                 n_median IN NUMBER,
                                   n_mode IN NUMBER,
                                    n_std IN NUMBER,
                                  i_layer IN INTEGER DEFAULT 0);
--
PROCEDURE ComputeImageStatistics( v_tablename IN VARCHAR2, i_imageid IN INTEGER);
--
PROCEDURE SetFilePrivileges( v_filename IN VARCHAR2);
PROCEDURE DBASetJAVAPolicy( v_schema IN VARCHAR2);
--
FUNCTION CreateRasterRDT    ( v_owner      IN VARCHAR2  DEFAULT USER) RETURN VARCHAR2;
FUNCTION GetTableRDT        ( v_tablename  IN VARCHAR2) RETURN VARCHAR2;
FUNCTION GetRasterColumn    ( v_tablename  IN VARCHAR2) RETURN VARCHAR2;
FUNCTION GetRasterExtentName( v_tablename  IN VARCHAR2) RETURN VARCHAR2;
FUNCTION GetImageSRID       ( v_tablename  IN VARCHAR2) RETURN INTEGER;
FUNCTION GetImageBands      ( v_tablename  IN VARCHAR2, i_imageid IN INTEGER) RETURN VARCHAR2;
FUNCTION GetImageBlocking   ( v_tablename  IN VARCHAR2, i_imageid IN INTEGER) RETURN VARCHAR2;
FUNCTION GetImageInterleave ( v_tablename  IN VARCHAR2, i_imageid IN INTEGER) RETURN VARCHAR2;
FUNCTION GetMaxImageOverview( v_tablename  IN VARCHAR2, i_imageid IN INTEGER) RETURN INTEGER;
FUNCTION GetImageCompression( v_tablename  IN VARCHAR2, i_imageid IN INTEGER) RETURN VARCHAR2;
FUNCTION ImageStorageParams ( v_blksize    IN VARCHAR2 DEFAULT NULL, 
                              v_interleave IN VARCHAR2 DEFAULT NULL,
                              v_compress   IN VARCHAR2 DEFAULT NULL,
                              i_quality    IN INTEGER  DEFAULT NULL,
                              i_srid       IN INTEGER  DEFAULT NULL) RETURN VARCHAR2;
--
PROCEDURE CreateStatisticsAll( v_owner     IN VARCHAR2 DEFAULT USER, 
                               i_sample    IN PLS_INTEGER DEFAULT 1000, 
                               b_overwrite IN BOOLEAN DEFAULT TRUE);
PROCEDURE CreateStatistics( i_rasterid  IN PLS_INTEGER, 
                            i_sample    IN PLS_INTEGER DEFAULT 1000, 
                            b_overwrite IN BOOLEAN DEFAULT FALSE);
--
END;
/
show errors
-- ----------------------------------------------------------------------------------------
-- ----------------------------------------------------------------------------------------
CREATE OR REPLACE PACKAGE BODY GDOSYS.GDORASTER IS
-- ----------------------------------------------------------------------------------------
-- DBA definable defaults, set these parameters to suit your needs:
-- ----------------------------------------------------------------------------------------
  c_rascolumn      CONSTANT VARCHAR2(30) := 'GEORASTER'; -- Raster Column Name
--
  c_rdttablespace  CONSTANT VARCHAR2(30) := 'RASTERS';   -- Raster Storage Tablespace
--
  c_uselobspace    CONSTANT VARCHAR2(3)  := 'YES';       -- Use LOBs (Yes) or standard storage?
--
  c_lobtablespace  CONSTANT VARCHAR2(30) := 'RASTERS';   -- Raster LOB Tablespace
--
  c_rdtchunk       CONSTANT INTEGER      := 32768;       -- RDT Chunk Size for I/O
--
  c_maxrdtsize     CONSTANT NUMBER       := 128;         -- Maximum Size of RDT in MB
--
  c_DefaultQuality CONSTANT INTEGER      := 85;          -- Default JPEG Compression Quality
--
  c_resampling     CONSTANT VARCHAR2(9)  := 'NN';        -- Default Pyramid Sampling 
--                                                       -- (NN, BILINEAR, AVERAGE4, AVERAGE16, CUBIC)
-- ----------------------------------------------------------------------------------------
-- Globals - Do not Change These
-- ----------------------------------------------------------------------------------------
  c_pkg_version    CONSTANT VARCHAR2(8) :='2016.001';     -- Current package version
  c_pkg_date       CONSTANT VARCHAR2(10):='07/17/2015';    -- Current package date

-- ----------------------------------------------------------------------------------------
-- Cursors
-- ----------------------------------------------------------------------------------------

   -- Get information about georaster based tables
   CURSOR GetGeoRasterTabs( v_owner VARCHAR2 DEFAULT USER) IS 
          SELECT table_name, column_name 
            FROM ALL_TAB_COLS 
           WHERE OWNER = v_owner AND data_type='SDO_GEORASTER' AND table_name NOT LIKE 'BIN$%';
   -- Get the ID's of the georaster objects
   CURSOR GetRasterIDs( v_owner VARCHAR2 DEFAULT USER) IS 
          SELECT RASTER_ID 
            FROM ALL_SDO_GEOR_SYSDATA 
           WHERE OWNER = v_owner;
   -- Get Raster object information from the raster object ID
   CURSOR GetRasterObj( v_rasterid INTEGER) IS 
          SELECT OWNER, TABLE_NAME, COLUMN_NAME 
            FROM ALL_SDO_GEOR_SYSDATA 
           WHERE RASTER_ID = v_rasterid;

-- ----------------------------------------------------------------------------------------
-- Global Exception Handling - You can edit msgs to reflect the local language.
-- ----------------------------------------------------------------------------------------

  e_TableNotFound       EXCEPTION;
  e_TableIsView         EXCEPTION;
  e_TabOwnerError       EXCEPTION;
  e_GeometryNotFound    EXCEPTION;
  e_RasterNotFound      EXCEPTION;
  e_InvalidRaster       EXCEPTION;
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
  c_msgRasterNotFound   CONSTANT VARCHAR2(70) := 'Georaster column not found (or too many exist): ';
  c_msgInvalidRaster    CONSTANT VARCHAR2(70) := 'Georaster column is invalid: ';
  c_msgSequenceNotFound CONSTANT VARCHAR2(70) := 'Specified sequence could not be found: ';
  c_msgNoDataFound      CONSTANT VARCHAR2(70) := 'Specified data could not be found: ';
  c_msgNoMetadataAccess CONSTANT VARCHAR2(70) := 'No insert/delete privileges on MDSYS.SDO_GEOM_METADATA_TABLE: ';
  c_msgNoMetadataFound  CONSTANT VARCHAR2(70) := 'No metadata entries found in ALL_GEOM_METADATA_TABLE for: ';
  c_msgNoPrivilege      CONSTANT VARCHAR2(70) := 'User does not have privilege for this operation.';
  c_msgInvalidDimension CONSTANT VARCHAR2(70) := 'The geometry dimensions are not valid for this operation.';
  c_msgTabSpaceNotFound CONSTANT VARCHAR2(70) := 'Specified tablespace does not exist: ';
  c_msgCannotBeNull     CONSTANT VARCHAR2(70) := 'Input Parameters Cannot be Null: ';
  -- Other standard msgs 
  c_msgNoKeyColumn      CONSTANT VARCHAR2(25) := 'No key column assigned.';
  c_msgOraError         CONSTANT VARCHAR2(25) := 'Oracle Error: ORA';
  c_msgStart            CONSTANT VARCHAR2(22) := 'Process Started for: ';
  c_msgComplete         CONSTANT VARCHAR2(22) := 'Process Complete for: ';
  c_msgGoomInternal     CONSTANT VARCHAR2(38) := 'GOOM - An Internal Error has occurred.';
  c_msgSRIDIsNull       CONSTANT VARCHAR2(46) := 'SRID Required for GeoRaster data: ';
  c_msgNoSIDX           CONSTANT VARCHAR2(42) := 'The table does NOT have a spatial index: ';
  -- Procedure Exception Responses
  c_msgError            CONSTANT VARCHAR2(10) := ' - ERROR';
  c_msgWarning          CONSTANT VARCHAR2(10) := ' - WARNING';
  c_msgVerify           CONSTANT VARCHAR2(10) := ' - VERIFY';
  c_msgInform           CONSTANT VARCHAR2(10) := ' - INFORM';
  
-- ----------------------------------------------------------------------------------------
-- Main Procedures
-- ----------------------------------------------------------------------------------------

  -- Version returns the package version number and date.
  -- Syntax: EXEC GDORASTER.VERSION;
  --
  PROCEDURE VERSION IS
    v_indexspace  VARCHAR2(30);
    c_support     CONSTANT VARCHAR2(14) := 'Via Email Only';
    c_emailpath   CONSTANT VARCHAR2(29) := 'chuck.woodbury@intergraph.com';
    c_onlinehelp  CONSTANT VARCHAR2(21) := 'EXEC GDORASTER.HELPME';
    i_length      PLS_INTEGER           := 66;
  BEGIN
    v_indexspace:=GOOM.GetGOOMIndexSpace;
    GOOM.DblLine(i_length);
    GOOM.TitleLine('INTERGRAPH',i_length,'**');
    GOOM.TitleLine('GeoMedia GeoRaster Package',i_length,'**');
    GOOM.TitleLine('GDORASTER PKG',i_length,'**');
    GOOM.DblLine(i_length);
    GOOM.TitleLine('Author: Chuck Woodbury, Senior Technical Consultant', i_length,'**');
    GOOM.TitleLine('Hexagon Geospatial Division', i_length,'**');
    GOOM.TitleLine('Intergraph Technology Services', i_length,'**');
    GOOM.DblLine(i_length);
    GOOM.Response('Version',c_pkg_version,i_length,TRUE);
    GOOM.Response('Date',c_pkg_date,i_length,TRUE);
    GOOM.Response('Spatial Index Tablespace',v_indexspace,i_length,TRUE);
    GOOM.Response('Bug Reports and Support',c_support,i_length,TRUE);
    GOOM.Response('Email',c_emailpath,i_length,TRUE);
    GOOM.Response('Online Help',c_onlinehelp,i_length,TRUE);
    GOOM.DblLine(i_length);
  END VERSION;
  -- ---------------------------------------------------------------------------
  -- HELPME returns package information and help.
  -- Syntax: EXEC GDORASTER.HELPME;
  --
  PROCEDURE HELPME( v_topic IN VARCHAR2 DEFAULT NULL) IS
    BEGIN
      IF v_topic IS NULL THEN
      GOOM.DblLine;
      GOOM.TitleLine('GDORASTER PACKAGE PROCEDURES SYNTAX   -   GDORASTER UTILITIES');
      GOOM.DblLine;
      GOOM.DBMSG('Enter EXEC GDORASTER.HELPME(''FUNCTIONS''); for GDORASTER function syntax.');
      GOOM.DBMSG('Run the following in to load and display a raster image in GeoMedia.      ');
      GOOM.DBMSG('DBAs should run the following to allow a user to load raster images:      ');
      GOOM.DBMSG('EXEC GDORASTER.DBASetJAVAPolicy(c_schema);  -- c_schema is the user name. ');
      GOOM.DashLine;
      GOOM.DBMSG('Step 1: Create a table to store raster images.                            '); 
      GOOM.DBMSG('- EXEC GDORASTER.CreateRasterTable(c_tablename);                          ');
      GOOM.DBMSG('- Note: To drop a raster table and all its associated objects (incl. RDTs)');
      GOOM.DBMSG('-       EXEC GDORASTER.DropRasterTable(c_tablename)                       ');
      GOOM.DBMSG('where:');
      GOOM.DBMSG('-   c_tablename  : The table name used to store the image.                ');
      GOOM.DBMSG('                                                                          ');
      GOOM.DBMSG('- If you are uing PCI''s Image Loader, use the following:                                                                          ');
      GOOM.DBMSG('- EXEC GDORASTER.CreatePCIRasterTable(v_tablename IN VARCHAR2);           ');
      GOOM.DotLine;
      GOOM.DBMSG('Step 2: Set the Java Privileges on files to read.  DBA only.');
      GOOM.DBMSG('- EXEC GDORASTER.SetFilePrivileges(c_filename);');
      GOOM.DBMSG('-   c_filename   : The full path to the file.');
      GOOM.DotLine;
      GOOM.DBMSG('Step 3: Load an individual image to the table specified in CreateRasterTable.');
      GOOM.DBMSG('- EXEC GDORASTER.LoadRasterImage(c_tablename,c_filename,c_worldfile,i_srid,');
      GOOM.DBMSG('-         c_desc,c_blocking,c_interleave,c_compression,i_quailty,c_update);');
      GOOM.DBMSG('where:');
      GOOM.DBMSG('-   c_tablename  : The table name used to store the image.');
      GOOM.DBMSG('-   c_filename   : The full path to the tiff file.');
      GOOM.DBMSG('-   c_worldfile  : The full path to the world file.');
      GOOM.DBMSG('-   i_srid       : The SRID for the image.' );
      GOOM.DBMSG('- The following parameters are optional:');
      GOOM.DBMSG('-   c_desc       : An optional description.');
      GOOM.DBMSG('-   c_blocking   : Blocking factor in the firm (n,n,b) eg. (256,256,3)    ');
      GOOM.DBMSG('-   c_interleave : Interleaving value - BSQ, BIP, BIL' );
      GOOM.DBMSG('-   c_compression: Compression value - JPEG-B, JPEG-F, DEFLATE, NONE ');
      GOOM.DBMSG('-   i_quality    : Compression quality (JPEG ONLY) 0-100 90 is default.');
      GOOM.DBMSG('-   c_update     : Update image attributes Y(default)/N                   ');
      GOOM.DBMSG('-                  Set to N if you are using your own table.              ');
      GOOM.DotLine;
      GOOM.DBMSG('Step 4: Apply a default MBR (before indexing).                            '); 
      GOOM.DBMSG('- EXEC GDORASTER.SetDefaultRasterMBR(c_tablename);                        ');
      GOOM.DBMSG('-   c_tablename  : The table name used to store the image.                ');
      GOOM.DBMSG('- EXEC GDORASTER.SetDefaultRasterMBR(c_tablename,''G'');                  ');
      GOOM.DBMSG('-   The G specifies this tif is geographic               .                ');
      GOOM.DotLine;
      GOOM.DBMSG('Step 5: Spatially index a raster table (after loading data).              '); 
      GOOM.DBMSG('- EXEC GDORASTER.CreateRasterIndex(c_tablename);                          ');
      GOOM.DBMSG('-   c_tablename  : The table name used to store the image.                ');
      GOOM.DBMSG('-   If no tablename is used all raster tables will be indexed.            ');
      GOOM.DotLine;
      GOOM.DBMSG('-- Other Utilities --');
      GOOM.DBMSG('Alter image with new storage parameters. Original image is deleted.');
      GOOM.DBMSG('- EXEC GDORASTER.AlterImageParameters(c_tablename,i_imageid,c_blocking,');
      GOOM.DBMSG('-                                     c_interleave,c_compression,c_update);');
      GOOM.DBMSG('Copy image with new storage parameters. Original image is preserved.');
      GOOM.DBMSG('- EXEC GDORASTER.CopyImageWithParameters(c_tablename,i_imageid,c_blocking,');
      GOOM.DBMSG('-                                        c_interleave,c_compression,c_desc,c_update);');   
      GOOM.DBMSG('-   c_tablename  : The table name used to store the image.                ');
      GOOM.DBMSG('-   i_imageid    : Primary Key ID of image to alter.                      ');
      GOOM.DBMSG('-   c_blocking   : Blocking factor in the firm (n,n,n) eg. (256,256,3)    ');
      GOOM.DBMSG('-   c_interleave : Interleaving: BSQ, BIP, BIL   Default is Source Image. ');
      GOOM.DBMSG('-   c_compression: Type: JPEG-B, JPEG-F, DEFLATE, NONE(default)           ');
      GOOM.DBMSG('-   c_desc       : Optionally add a description (Copy Only)               ');
      GOOM.DBMSG('-   c_update     : Update image attributes Y(default)/N                   ');
      GOOM.DBMSG('Set or alter the overview pyramids for an for existing image.             ');
      GOOM.DBMSG('- EXEC GDORASTER.SetImagePyramid(c_tablename,i_imageid,i_level,c_method,c_update);');
      GOOM.DBMSG('-   i_level      : maximum number of overviews, NULL (default) for automatic.  ');
      GOOM.DBMSG('-   c_method     : Resampling: AVERAGE4(default), AVERAGE16, NN, CUBIC' );
      GOOM.DBMSG('-   c_update     : Update image attributes Y(default)/N ');
      GOOM.DBMSG('Set or alter the statistics for an for existing image:             ');
      GOOM.DBMSG('- EXEC GDORASTER.SetImageStatistics(c_tablename,i_imageid,n_min,n_max,'    );
      GOOM.DBMSG('-                                   n_median,n_mode,n_std,i_layer);'       );
      GOOM.DBMSG('-   n_min,n_max  : min/max pixel values.  ');
      GOOM.DBMSG('-   n_median     : Median value.' );
      GOOM.DBMSG('-   n_mode       : Mode value.' );
      GOOM.DBMSG('-   n_std        : Standard Dev value.' );
      GOOM.DBMSG('-   i_layer      : Pyramid level to set. Default is 0, actual image.');
      GOOM.DBMSG('Update Image Attributes with current image storage parameters.');
      GOOM.DBMSG('- EXEC GDORASTER.UpdateImageAttributes(c_tablename, i_imageid);');
      GOOM.DBLLINE;
      ELSE
      GOOM.DBLLINE;
      GOOM.TitleLine('GDORASTER PACKAGE FUNCTION SYNTAX   -   GDORASTER UTILITIES');
      GOOM.DBLLINE;
      GOOM.DBMSG('Enter EXEC GDORASTER.HELPME; for GDORASTER procedure syntax.');
      GOOM.DashLine;
      GOOM.DBMSG('Return the name of the RDT table for the specific raster table if it exists,');
      GOOM.DBMSG('- create and initialize the RDT table first if it does not exist.');
      GOOM.DBMSG('- v_table:=gdoraster.GetTableRDT(c_tablename);');
      GOOM.DBMSG('Return the name of the column containing the raster image.');
      GOOM.DBMSG('- SELECT GDORASTER.GetRasterColumn(c_tablename) FROM DUAL;');
      GOOM.DBMSG('Return the name of the column containing the raster extent.');
      GOOM.DBMSG('- SELECT GDORASTER.GetRasterExtentName(c_tablename) FROM DUAL;');
      GOOM.DBMSG('Return the SRID of the assigned raster table');
      GOOM.DBMSG('- SELECT GDORASTER.GetImageSRID(c_tablename) FROM DUAL;');
      GOOM.DBMSG('Return the primary key column name for the raster table.');
      GOOM.DBMSG('- SELECT GOOM.GetKeyCol(c_tablename) FROM DUAL;');
      GOOM.DBMSG('Return the number of image bands.');
      GOOM.DBMSG('- SELECT GDORASTER.GetImageBands (c_tablename, i_imageid) FROM DUAL;');
      GOOM.DBMSG('Return the image blocking value.');
      GOOM.DBMSG('- SELECT GDORASTER.GetImageBlocking (c_tablename, i_imageid) FROM DUAL;');
      GOOM.DBMSG('Return the image interleaving value.');
      GOOM.DBMSG('- SELECT GDORASTER.GetImageInterleave (c_tablename, i_imageid) FROM DUAL;');
      GOOM.DBMSG('Return the maximum number of overview pyramids in image.');
      GOOM.DBMSG('- SELECT GDORASTER.GetMaxImageOverview (c_tablename, i_imageid) FROM DUAL;');
      GOOM.DBMSG('where:');
      GOOM.DBMSG('-   c_tablename is the raster table name.');
      GOOM.DBMSG('-   i_pid is the primary key value for the specific image.');
      GOOM.DBMSG('Return the storage parameter string to use in import, alter, and copy.');
      GOOM.DBMSG('- SELECT GDORASTER.ImageStorageParams(c_blksize,c_interleave,c_compress,i_quality) FROM DUAL;');
      GOOM.DBMSG('where:');
      GOOM.DBMSG('-   c_blocking   : Blocking factor in the firm (n,n,b) eg. (256,256,3)    ');
      GOOM.DBMSG('-   c_interleave : Interleaving value - BSQ, BIP, BIL' );
      GOOM.DBMSG('-   c_compression: Compression value - JPEG-B, JPEG-F, DEFLATE, NONE ');
      GOOM.DBMSG('-   i_quality    : Compression quality (JPEG ONLY) 0-100 90 is default.');
      GOOM.DBLLINE;
      END IF;
  END HELPME;
  -- ---------------------------------------------------------------------------
  -- Version11 returns boolean TRUE if the DB is version 11 or later.
  -- Syntax: IF GDORASTER.Version11 THEN...
  --         IF NOT GDORASTER.Version11 THEN...
  --
  FUNCTION Version11 RETURN BOOLEAN IS
  i_ver   PLS_INTEGER;
  BEGIN
    i_ver := SUBSTR(GOOM.GETDBVERSION,0,2);
    IF i_ver >= 11 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END IF;
  END Version11;
  -- ---------------------------------------------------------------------------
  -- DropRasterTable drops a raster based table and all its components.
  -- Syntax: EXEC GDORASTER.DropTasterTable(v_tablename);
  --         EXEC GDORASTER.DropTasterTable('<table_name>');
  --  
  PROCEDURE DropRasterTable( v_tablename IN VARCHAR2) IS
    c_cmdname       CONSTANT VARCHAR2(15) := 'DropRasterTable';
    c_feedback      CONSTANT VARCHAR2(30) := 'Dropping Raster Data Table: ';
    c_feedback2     CONSTANT VARCHAR2(30) := 'GeoRaster Table Dropped: ';
    --
    v_rdt_table  VARCHAR2(30);
    v_cursor     PLS_INTEGER;
    v_Dum        PLS_INTEGER;
    v_sql        VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30);  
  BEGIN
    v_ownertable := GOOM.GetOwnerObject(v_tablename);
    v_owner      := GOOM.SplitOwnerObject(v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject(v_ownertable,'TABLE');
    IF NOT GOOM.ChkTable(v_ownertable) THEN
      RAISE e_TableNotFound;
    END IF; 
    IF NOT Version11 THEN
      v_sql :='SELECT RDT_TABLE_NAME FROM ALL_SDO_GEOR_SYSDATA WHERE OWNER='''||v_owner||''' AND TABLE_NAME ='''||v_table||'''';
      v_cursor := SYS.DBMS_SQL.OPEN_CURSOR;
      SYS.DBMS_SQL.PARSE( v_cursor, v_sql, SYS.DBMS_SQL.NATIVE);
      SYS.DBMS_SQL.DEFINE_COLUMN( v_cursor, 1, v_rdt_table, 30);
      v_Dum := SYS.DBMS_SQL.EXECUTE( v_cursor );
      LOOP
          IF( SYS.DBMS_SQL.FETCH_ROWS(v_cursor) = 0 ) THEN
             EXIT;
          END IF;
          SYS.DBMS_SQL.COLUMN_VALUE( v_cursor, 1, v_rdt_table );
          EXECUTE IMMEDIATE 'DROP TABLE '||v_owner||'.'||v_rdt_table||' PURGE';
          GOOM.Response(c_cmdname,c_feedback||v_owner||'.'||v_rdt_table);
      END LOOP;        
      SYS.DBMS_SQL.CLOSE_CURSOR( v_cursor );
      GOOM.DropTable(v_ownertable);
      EXECUTE IMMEDIATE 'PURGE RECYCLEBIN';
    ELSE 
      EXECUTE IMMEDIATE 'DROP TABLE '||v_ownertable||' PURGE';
    END IF;
    GOOM.Response(c_cmdname,c_feedback2||v_ownertable);
    EXCEPTION
      WHEN e_TableNotFound THEN
        GOOM.Response(c_cmdname||c_msgWarning,c_msgTableNotFound||v_ownertable);
      WHEN OTHERS THEN
        GOOM.REPORT_ERROR (c_cmdname,v_ownertable,v_rdt_table,sqlcode,sqlerrm);
  END DropRasterTable;
-- ---------------------------------------------------------------------------
  -- CreateRasterTable creates a default raster based table with all required objects.
  -- Syntax: EXEC GDORASTER.CreateRasterTable(v_tablename);
  --         EXEC GDORASTER.CreateRasterTable('<table_name>');
  --  
  PROCEDURE CreateRasterTable( v_tablename IN VARCHAR2) IS
    c_cmdname       CONSTANT VARCHAR2(17) := 'CreateRasterTable';
    c_created       CONSTANT VARCHAR2(30) := 'GeoRaster table created: ';
    c_trigmsg       CONSTANT VARCHAR2(35) := 'Raster DML trigger created in: ';
    --
    v_seqname       VARCHAR2(61);
    v_rascolumn     VARCHAR2(30);
    v_sql           VARCHAR2(512);
    v_debug         VARCHAR2(32):='Init';
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30);   
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_owner      := GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    IF GOOM.chkTable(v_ownertable) THEN
      DropRasterTable(v_ownertable);
    END IF;
    -- Current table definition.
    v_sql := 'CREATE TABLE '|| v_ownertable ||'(ID INTEGER PRIMARY KEY, IMAGE_NAME VARCHAR2(64), DESCRIPTION VARCHAR2(255),CS_NAME VARCHAR2(80),
             BLOCKING VARCHAR2(16), INTERLEAVE VARCHAR2(7), COMPRESSION VARCHAR2(7), MAX_OVERVIEW INTEGER, DATE_LOADED DATE, GEORASTER MDSYS.SDO_GEORASTER)';
    EXECUTE IMMEDIATE v_sql;  
    --
    GOOM.Response( c_cmdname, c_created || v_ownertable);
    GOOM.CreateNewSequence( v_ownertable, NULL, v_seqname);
    v_rascolumn := GetRasterColumn( v_ownertable );
    IF NOT Version11 THEN
      v_debug:='Create DML';
      SDO_GEOR_UTL.createDMLTrigger( v_table, v_rascolumn);
      GOOM.Response( c_cmdname, c_trigmsg || v_owner);
    END IF;
    v_debug:='Create Metadata';
    GOOM.SetGDOSYSMetadata( v_tablename, v_seqname);
    EXCEPTION
      WHEN OTHERS THEN
        GOOM.REPORT_ERROR ( c_cmdname, v_ownertable, v_sql, v_debug, sqlerrm);
  END CreateRasterTable;
-- ---------------------------------------------------------------------------
  -- CreatePCIRasterTable creates a default raster based table for the PCI GeoRaster Loader.
  -- Syntax: EXEC GDORASTER.CreatePCIRasterTable(v_tablename);
  --         EXEC GDORASTER.CreatePCIRasterTable('<table_name>');
  --
  PROCEDURE CreatePCIRasterTable( v_tablename IN VARCHAR2) IS
    c_cmdname       CONSTANT VARCHAR2(20) := 'CreatePCIRasterTable';
    c_feedback      CONSTANT VARCHAR2(30) := 'PCI Raster table created: ';
    c_feedback2     CONSTANT VARCHAR2(35) := 'Raster DML trigger created in: ';
  --
    v_seqname       VARCHAR2(61);
    v_rascolumn     VARCHAR2(30);
    v_rdt           VARCHAR2(60);
    v_sql           VARCHAR2(512);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30);  
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_owner      := GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    IF GOOM.chkTable( v_ownertable) THEN
      DropRasterTable( v_ownertable);
    END IF;
    -- Current table definition.
    v_sql:='CREATE TABLE '|| v_ownertable ||'(ID INTEGER PRIMARY KEY, IMAGE_NAME VARCHAR2(64), DESCRIPTION VARCHAR2(255),PCS_CITATION VARCHAR2(80),
            BLOCKING VARCHAR2(16), INTERLEAVE VARCHAR2(7),MAX_OVERVIEW INTEGER, DATE_LOADED DATE DEFAULT SYSDATE, DATUM VARCHAR2(30),ISHIC INTEGER,GEORASTER MDSYS.SDO_GEORASTER)';
    EXECUTE IMMEDIATE v_sql;  
    --
    GOOM.Response( c_cmdname, c_feedback || v_ownertable);
    GOOM.CreateNewSequence( v_ownertable,NULL, v_seqname);
    v_rascolumn:=GetRasterColumn( v_ownertable);
    IF NOT Version11 THEN
      SDO_GEOR_UTL.createDMLTrigger( v_ownertable, v_rascolumn);
      GOOM.Response( c_cmdname, c_feedback2 || v_owner);
    END IF;
    v_rdt := GetTableRDT( v_ownertable );
    GOOM.Response('FOR PCI LOADER','Use RDT: '|| v_rdt ||' with table '|| v_ownertable ||'.');
    GOOM.SetGDOSYSMetadata( v_ownertable, v_seqname);
    v_sql:='CREATE OR REPLACE TRIGGER '|| v_ownertable ||'_TRG BEFORE INSERT ON '|| v_ownertable ||' FOR EACH ROW
            BEGIN
              IF :new.ID IS NULL THEN 
                select '|| v_seqname ||'.nextval into :new.ID from dual;    
              END IF;
           END;';
    EXECUTE IMMEDIATE v_sql;
    GOOM.Response('TRIGGER CREATED','Trigger used for PCI uploads - '|| v_ownertable ||'_TRG.');
    EXCEPTION
      WHEN OTHERS THEN
        GOOM.REPORT_ERROR ( c_cmdname, v_ownertable, v_sql,'RDT:'|| v_rdt, sqlerrm);
  END CreatePCIRasterTable;
-- ---------------------------------------------------------------------------
  -- CreateRasterIndex creates create the required georaster spatial indexes.
  -- Syntax: EXEC GDORASTER.CreateRasterIndex(v_tablename);
  --         EXEC GDORASTER.CreateRasterIndex('<table_name>');
  --
  PROCEDURE CreateRasterIndex( v_tablename IN VARCHAR2 DEFAULT NULL) IS
    c_cmdname       CONSTANT VARCHAR2(17):='CreateRasterIndex';
    --
    v_raster        GetGeoRasterTabs%ROWTYPE;
    v_rastab        VARCHAR2(61);
    v_rasextent     VARCHAR2(128);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_rasextent  :=GetRasterExtentName( v_ownertable);
    GOOM.RTree( v_ownertable, v_rasextent);
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_ownertable, v_rasextent, sqlcode, sqlerrm);
  END CreateRasterIndex;
-- ---------------------------------------------------------------------------
  -- RasterIndexAll creates create the required georaster spatial indexes for all tables.
  -- Syntax: EXEC GDORASTER.RasterIndexAll(v_owner);
  --         EXEC GDORASTER.RasterIndexAll('<v_owner>');
  --  
  PROCEDURE RasterIndexAll( v_owner IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(15):='RasterIndexAll';
    --
    v_raster        GetGeoRasterTabs%ROWTYPE;
    v_rastable      VARCHAR2(61);
    v_rasextent     VARCHAR2(128);
    -- added for owner.table support
  BEGIN
    GOOM.Response( c_cmdname, c_msgStart || v_owner,28);
    FOR v_raster IN GetGeoRasterTabs( v_owner) LOOP
      v_rastable := v_owner||'.'|| v_raster.table_name;
      v_rasextent:= v_raster.column_name||'.SPATIALEXTENT';
      GOOM.RTree( v_rastable, v_rasextent);
    END LOOP;
    GOOM.Response( c_cmdname, c_msgComplete|| v_owner,28);
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_rastable, v_rasextent, sqlcode, sqlerrm);
  END RasterIndexAll;
-- ---------------------------------------------------------------------------
  -- SetRasterExtents uses SDO_GEOR.GenerateSpatialExtent to generate the required raster extents.
  -- Syntax: EXEC GDORASTER.SetRasterExtents(v_tablename, v_id);
  --         v_tablename : the georaster table to process
  --         v_id        : the primary key of the row to set the extents for.
  --  
  PROCEDURE SetRasterExtents( v_tablename IN VARCHAR2, v_id IN INTEGER DEFAULT NULL) IS
    c_cmdname      CONSTANT VARCHAR2(16):='SetRasterExtents';
    c_feedback     CONSTANT VARCHAR2(30):='Spatial Extents set for: ';
    --
    v_rascolumn    VARCHAR2(30);
    v_rasextent    VARCHAR2(64);
    v_pkey         VARCHAR2(32);
    i_imageid      INTEGER;
    v_sql          VARCHAR2(512);
    v_query        VARCHAR2(512);
    v_cursor       PLS_INTEGER;
    v_dum          PLS_INTEGER;
    v_debug        VARCHAR2(32):='Init';
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30);  
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_owner      := GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    v_rascolumn  := GetRasterColumn( v_ownertable);
    v_rasextent  := GetRasterExtentName( v_ownertable);
    v_pkey       := GOOM.GetKeyCol( v_ownertable);
    IF v_id IS NULL THEN
      v_query  := 'SELECT '|| v_pkey ||' FROM '|| v_ownertable;
      v_cursor := SYS.DBMS_SQL.OPEN_CURSOR;
      SYS.DBMS_SQL.PARSE( v_cursor, v_query, SYS.DBMS_SQL.NATIVE);
      SYS.DBMS_SQL.DEFINE_COLUMN( v_cursor, 1, i_imageid);
      v_Dum := SYS.DBMS_SQL.EXECUTE( v_cursor );
      LOOP
        IF (SYS.DBMS_SQL.FETCH_ROWS( v_cursor) > 0) THEN
          SYS.DBMS_SQL.COLUMN_VALUE( v_cursor, 1, i_imageid);
          v_sql:='UPDATE '|| v_ownertable ||' A SET A.'|| v_rasextent ||'= SDO_GEOR.GenerateSpatialExtent('|| v_rascolumn ||') WHERE A.'|| v_pkey ||'='|| i_imageid ;
          EXECUTE IMMEDIATE v_sql;      
          v_debug := 'In Loop';
        ELSE
          EXIT;
        END IF;
      END LOOP;
      SYS.DBMS_SQL.CLOSE_CURSOR( v_cursor );
    ELSE
      v_sql:='UPDATE '|| v_ownertable ||' A SET A.'|| v_rasextent ||'= SDO_GEOR.GenerateSpatialExtent('|| v_rascolumn ||') WHERE A.'|| v_pkey ||'='|| v_id;
      EXECUTE IMMEDIATE v_sql;    
    END IF;
    COMMIT;
    GOOM.Response( c_cmdname, c_feedback || v_ownertable ||'.'|| v_rascolumn ||'.');
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_debug, sqlcode, sqlerrm);
      SYS.DBMS_SQL.CLOSE_CURSOR( v_cursor );
  END SetRasterExtents;
-- ---------------------------------------------------------------------------
  -- SetDefaultRasterMBR sets default extents in USER_SDO_GEOM_METADATA based on type.
  -- Syntax: EXEC GDORASTER.SetDefaultRasterMBR(v_tablename, v_typ);
  --         v_tablename : the georaster table to process
  --         v_typ       : 'P' for projected and 'G' for geographic.
  --
  PROCEDURE SetDefaultRasterMBR( v_tablename IN VARCHAR2, v_typ IN VARCHAR2 DEFAULT 'P') IS
    c_cmdname       CONSTANT VARCHAR2(19):='SetDefaultRasterMBR';
    v_rasextent     VARCHAR2(64);
    i_srid          INTEGER;
    v_debug         VARCHAR2(32):='Init';
    v_ownertable    VARCHAR2(61);
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename );
    v_rasextent  := GetRasterExtentName( v_ownertable );
    i_srid       := GetImageSRID( v_ownertable );
    IF GOOM.isGeographic( i_srid ) OR v_typ ='G' THEN
      IF i_srid=0 THEN 
        GOOM.InsertMBR(2, v_ownertable, v_rasextent,-90,90,-180,180,0.05,NULL); 
        GOOM.Response( c_cmdname || c_msgWarning, c_msgSRIDIsNull || v_ownertable ||'.'|| v_rasextent);
      ELSE
        GOOM.InsertMBR(2, v_ownertable, v_rasextent,-90,90,-180,180,0.05, i_srid);
      END IF;
    ELSE
      IF i_srid=0 THEN 
        GOOM.InsertMBR(2, v_ownertable, v_rasextent,-2147483648,2147483648,-2147483648,2147483648,0.00005,NULL); 
        GOOM.Response( c_cmdname || c_msgWarning, c_msgSRIDIsNull || v_ownertable ||'.'|| v_rasextent);
      ELSE
        GOOM.InsertMBR(2, v_ownertable, v_rasextent,-2147483648,2147483648,-2147483648,2147483648,0.00005, i_srid);
      END IF;
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_rasextent, v_debug, sqlcode, sqlerrm);
  END SetDefaultRasterMBR;
-- ---------------------------------------------------------------------------
  -- LoadRasterImage loades the specified raster file to the database..
  -- Syntax: EXEC GDORASTER.LoadRasterImage(v_tablename,v_filename,v_worldfile,i_srid,v_desc,
  --                                        v_blocking,v_interleave,v_compression,i_quailty,v_update);
  --  c_tablename  : The table name used to store the image.
  --  c_filename   : The full path to the tiff file.
  --  c_worldfile  : The full path to the world file.
  --  i_srid       : The SRID for the image.
  -- The following parameters are optional:
  --  v_desc       : An optional description.
  --  v_blocking   : Blocking factor in the form (n,n,b) eg. (256,256,3) 
  --  v_interleave : Interleaving value - BSQ, BIP, BIL' );
  --  v_compression: Compression value - JPEG-B, JPEG-F, DEFLATE, NONE 
  --  i_quality    : Compression quality (JPEG ONLY) 0-100 90 is default.
  --  v_update     : Update image attributes Y(default)/N   Set to N if you are using your own table.          
 
  PROCEDURE LoadRasterImage( v_tablename IN VARCHAR2, 
                              v_filename IN VARCHAR2, 
                             v_worldfile IN VARCHAR2, 
                                  i_srid IN INTEGER, 
                                  v_desc IN VARCHAR2 DEFAULT NULL,
                               v_blksize IN VARCHAR2 DEFAULT NULL,
                              v_compress IN VARCHAR2 DEFAULT 'NONE',
                               i_quality IN INTEGER  DEFAULT NULL,
                                v_update IN VARCHAR2 DEFAULT 'Y') IS
  -- Constants
    c_cmdname    CONSTANT VARCHAR2(30):='LoadRasterImage';
    c_imgtype    CONSTANT VARCHAR2(4) :='TIFF';           --TIFF, GIF, BMP, GeoTIFF, or PNG
    c_filetype   CONSTANT VARCHAR2(4) :='FILE';
    c_worldtype  CONSTANT VARCHAR2(9) :='WORLDFILE';
  -- Variables
    v_seqname    VARCHAR2(30);
    v_rdt        VARCHAR2(61);
    v_rascolumn  VARCHAR2(30);
    v_blocking   VARCHAR2(30);
    n_maxov      INTEGER:=0;
    v_parameters VARCHAR2(256):=NULL;
    v_interleave VARCHAR2(3);
    v_pkey       VARCHAR2(30);
  --
    v_file       VARCHAR2(255);
    v_geor       MDSYS.SDO_GEORASTER;
    i_imageid    INTEGER;
    v_sql        VARCHAR2(512);
    v_debug      VARCHAR2(255);
    v_descrip    VARCHAR2(255);
    v_csname     VARCHAR2(80);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30); 
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_owner      := GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    -- Collect general parameters.
    v_rdt      := GetTableRDT( v_ownertable);                       -- Get the name of the raster data table, create one if it does not exist.
    v_rascolumn:= GetRasterColumn( v_ownertable);                   -- Get the raster column name.
    v_seqname  := GOOM.GetSequenceName( v_ownertable);              -- Get the pkey sequence name (assuming tabname_sq syntax).
    v_pkey     := GOOM.GetKeyCol( v_ownertable);                    -- Get the primary key column (assuming only one).
    v_csname   := GOOM.GetCSName( i_srid);                          -- Get the CS name from the SRID.
    v_file     := SUBSTR( v_filename,INSTR( v_filename,'\',-1)+1);  -- Get the file name loaded.
    -- Build the image storage parameters.
    v_parameters:=ImageStorageParams( v_blksize, v_interleave, v_compress, i_quality, i_srid); 
    -- Build the description. 
    IF v_desc IS NULL THEN
      v_descrip := v_parameters;
    END IF;
    -- Initialize empty georaster objects to which the external images are to be imported
    v_debug := 'Initialize empty georaster';
    v_sql   := 'INSERT INTO '|| v_ownertable ||'('|| v_pkey ||',GEORASTER) VALUES('|| v_seqname ||'.nextval, SDO_GEOR.INIT('''|| v_rdt ||'''))';  
    EXECUTE IMMEDIATE v_sql;
    v_debug :='Get ID of current empty image row.';
    v_sql   :='SELECT MAX('|| v_pkey ||') FROM '|| v_ownertable;
    EXECUTE IMMEDIATE v_sql INTO i_imageid;
    v_debug :='Extract raster image holder.';
    v_sql   :='SELECT '|| c_rascolumn ||' FROM '|| v_ownertable ||' WHERE '|| v_pkey ||' = '|| i_imageid ||' FOR UPDATE';
    EXECUTE IMMEDIATE v_sql INTO v_geor;
    v_debug :='Beginning SDO_GEOR.importFrom - '|| v_parameters;
    GOOM.Response(c_cmdname,v_debug);
    SDO_GEOR.importFrom( v_geor, v_parameters, c_imgtype, c_filetype, v_filename, c_worldtype, c_filetype, v_worldfile);
    v_debug :='Import of '|| v_file ||' complete.';
    GOOM.Response(c_cmdname,v_debug);
    -- Now process the image holder to set required parameters.
    -- Set the SRID to georeference the image.
    SDO_GEOR.setModelSRID( v_geor, i_srid);
    v_debug:='Set SRID:'||i_srid||' complete.';
    GOOM.Response( c_cmdname, v_debug);
    -- Set the image to spatially referenced.
    SDO_GEOR.setSpatialReferenced( v_geor,'TRUE');
    v_debug:='Image is spatially referenced.';
    GOOM.Response( c_cmdname, v_debug);
    -- Generate default overview pyramids using default resampling.
    SDO_GEOR.GENERATEPYRAMID( v_geor,'resampling='|| c_resampling);
    v_debug:='Default overview pyramid generation complete.';
    GOOM.Response( c_cmdname, v_debug);
    -- Now load the raster object back into the table and commit the results.
    v_sql:='UPDATE '|| v_ownertable ||' SET '|| v_rascolumn ||' = :vgeor WHERE '|| v_pkey ||' = '|| i_imageid;
    EXECUTE IMMEDIATE v_sql USING v_geor;
    COMMIT;
    v_debug:='Image '|| v_file ||' loaded into '|| v_ownertable ||' using '|| v_pkey ||'='|| i_imageid ||'.';
    GOOM.Response( c_cmdname, v_debug);
    -- Set the spatial extents for the row.
    SetRasterExtents( v_ownertable, i_imageid);
    -- Optionally update the image attribute columns if you used CreateRasterTable.
    IF v_update = 'Y' THEN
      v_sql := 'UPDATE '|| v_ownertable ||' SET IMAGE_NAME='''|| v_file ||''' WHERE '|| v_pkey ||' = '|| i_imageid;
      EXECUTE IMMEDIATE v_sql;
      v_sql := 'UPDATE '|| v_ownertable ||' SET DESCRIPTION='''|| v_descrip ||''' WHERE '|| v_pkey ||' = '|| i_imageid;
      EXECUTE IMMEDIATE v_sql;
      v_sql := 'UPDATE '|| v_ownertable ||' SET CS_NAME='''|| v_csname ||''' WHERE '|| v_pkey ||' = '|| i_imageid;
      EXECUTE IMMEDIATE v_sql;
      v_sql := 'UPDATE '|| v_ownertable ||' SET DATE_LOADED='''|| SYSDATE ||''' WHERE '|| v_pkey ||' = '|| i_imageid;
      EXECUTE IMMEDIATE v_sql;
      COMMIT;
      v_debug:='Updated Basic Image Attributes in '|| v_ownertable ||'.'|| v_pkey ||'='|| i_imageid ||'.';
      GOOM.Response('UPDATE ATTRIBUTES', v_debug);
      UpdateImageAttributes( v_ownertable, i_imageid);
    END IF;
    EXCEPTION
      WHEN OTHERS THEN
        GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_filename, v_debug,sqlerrm);
  END LoadRasterImage;
-- ---------------------------------------------------------------------------
  -- UpdateImageAttributes sets the attributes of the GDORASTER built image table to 
  -- refect the parameters of the stored image referenced by the imageid.
  -- Syntax: EXEC GDORASTER.UpdateImageAttributes(v_tablename, i_imageid);
  -- v_tablename : the georaster table to process
  -- i_imageid   : the image id to collect information from.
  --
  PROCEDURE UpdateImageAttributes( v_tablename IN VARCHAR2, i_imageid IN INTEGER) IS
    c_cmdname     CONSTANT VARCHAR2(21):='UpdateImageAttributes';
    c_operation   CONSTANT VARCHAR2(17):='UPDATE ATTRIBUTES';
    v_blocking    VARCHAR2(16);
    v_interleave  VARCHAR2(3);
    v_compress    VARCHAR2(8);
    n_maxov       INTEGER:=0;
    v_pkey        VARCHAR2(30);
    v_sql        VARCHAR2(512);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30); 
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_owner      := GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    --
    v_pkey       :=GOOM.GetKeyCol( v_ownertable);
    v_blocking   :=GetImageBlocking( v_ownertable, i_imageid);
    v_interleave :=GetImageInterleave( v_ownertable, i_imageid);
    v_compress   :=GetImageCompression( v_ownertable, i_imageid);
    n_maxov      :=GetMaxImageOverview( v_ownertable, i_imageid);
    v_sql        :='UPDATE '|| v_ownertable ||' SET BLOCKING='''|| v_blocking ||''' WHERE '|| v_pkey ||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql;
    v_sql := 'UPDATE '|| v_ownertable ||' SET INTERLEAVE='''|| v_interleave ||''' WHERE '|| v_pkey ||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql;
    v_sql := 'UPDATE '|| v_ownertable ||' SET COMPRESSION='''|| v_compress ||''' WHERE '|| v_pkey ||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql;
    v_sql := 'UPDATE '|| v_ownertable ||' SET MAX_OVERVIEW='|| n_maxov ||' WHERE '|| v_pkey ||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql;
    COMMIT;
    GOOM.Response(c_cmdname,'Updated Image Storage Attributes in '|| v_ownertable ||'.'|| v_pkey ||'='|| i_imageid ||'.');
  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_ownertable, i_imageid, sqlerrm);
  END UpdateImageAttributes;
-- ---------------------------------------------------------------------------
  -- AlterImageParameters changes the storage parameters for an image referenced by 
  -- the imageid for the indicated georaster table.  The original image is deleted.
  -- Syntax: EXEC GDORASTER.AlterImageParameters(v_tablename,i_imageid,v_blocking,
  --                               v_interleave,v_compression,i_quality,v_update);
  --  v_tablename  : The table name used to store the image.
  --  i_imageid    : the image id to collect information from.
  --  v_blocking   : Blocking factor in the firm (n,n,b) eg. (256,256,3) 
  --  v_interleave : Interleaving value - BSQ, BIP, BIL' );
  --  v_compression: Compression value - JPEG-B, JPEG-F, DEFLATE, NONE 
  --  i_quality    : Compression quality (JPEG ONLY) 0-100 90 is default.
  --  v_update     : Update image attributes Y(default)/N   Set to N if you are using your own table. 
  --   
  PROCEDURE AlterImageParameters( v_tablename IN VARCHAR2, 
                                    i_imageid IN INTEGER, 
                                   v_blocking IN VARCHAR2, 
                                 v_interleave IN VARCHAR2 DEFAULT NULL,
                                v_compression IN VARCHAR2 DEFAULT 'NONE',
                                    i_quality IN INTEGER  DEFAULT NULL,
                                     v_update IN VARCHAR2 DEFAULT 'Y') IS
    --
    c_cmdname       CONSTANT VARCHAR2(20):='AlterImageParameters';
    v_pkey          VARCHAR2(30);
    v_debug         VARCHAR2(255):='Init';
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename );
    --
    GOOM.RESPONSE( c_cmdname, c_msgStart||' Original Image ID = '|| i_imageid);
    CopyImageWithParameters( v_ownertable, i_imageid, v_blocking, v_interleave, v_compression, i_quality, v_update);
    v_pkey:=GOOM.GetKeyCol( v_ownertable);
    EXECUTE IMMEDIATE 'DELETE FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'='|| i_imageid;
    COMMIT;
    GOOM.RESPONSE( c_cmdname, c_msgComplete ||' Deleted Image ID = '|| i_imageid ||'.');
  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      GOOM.REPORT_ERROR ( c_cmdname, v_ownertable, i_imageid, v_blocking,sqlerrm);
  END AlterImageParameters;
-- ---------------------------------------------------------------------------
  -- CopyImageWithParameters changes the storage parameters for an image referenced by 
  -- the imageid for the indicated georaster table.  The original image is preserved.
  -- Syntax: EXEC GDORASTER.CopyImageWithParameters(v_tablename,i_imageid,v_blocking,
  --                                  v_interleave,v_compression,i_quality,v_update);
  --  v_tablename  : The table name used to store the image.
  --  i_imageid    : the image id to collect information from.
  --  v_blocking   : Blocking factor in the firm (n,n,b) eg. (256,256,3) 
  --  v_interleave : Interleaving value - BSQ, BIP, BIL' );
  --  v_compression: Compression value - JPEG-B, JPEG-F, DEFLATE, NONE 
  --  i_quality    : Compression quality (JPEG ONLY) 0-100 90 is default.
  --  v_update     : Update image attributes Y(default)/N   Set to N if you are using your own table. 
  --  
  PROCEDURE CopyImageWithParameters( v_tablename IN VARCHAR2, 
                                       i_imageid IN INTEGER, 
                                      v_blocking IN VARCHAR2, 
                                    v_interleave IN VARCHAR2 DEFAULT NULL,
                                   v_compression IN VARCHAR2 DEFAULT 'NONE',
                                       i_quality IN INTEGER DEFAULT NULL,
                                        v_update IN VARCHAR2 DEFAULT 'Y') IS
  --
    c_cmdname       CONSTANT VARCHAR2(23):='CopyImageWithParameters';
    c_operation     CONSTANT VARCHAR2(10):='COPY IMAGE';
    v_rascolumn     VARCHAR2(30);
    v_georIn        sdo_georaster;
    v_georOut       sdo_georaster;
    v_sql           VARCHAR2(512);
    v_parameters    VARCHAR2(255);
    v_debug         VARCHAR2(255):='Init';
    n_maxid         INTEGER;
    v_rdt           VARCHAR2(30);
    v_seq           VARCHAR2(61);
    v_imagename     VARCHAR2(64);
    v_pkey          VARCHAR2(30);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30); 
  --   
  BEGIN
    -- Set parameters
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_owner      := GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    v_pkey       := GOOM.GetKeyCol( v_ownertable);
    v_rascolumn  := GetRasterColumn( v_ownertable);
    v_rdt        := GetTableRDT( v_ownertable);
    v_seq        := GOOM.GetSequenceName( v_ownertable);
    -- Set the storage parameters
    v_parameters := ImageStorageParams( v_blocking, v_interleave, v_compression, i_quality);
    v_sql  :='SELECT '|| v_rascolumn ||' FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql INTO v_georIn;
    v_debug:='Retrieved image '|| v_imagename ||'.';
    GOOM.RESPONSE( c_cmdname, v_debug);
    -- Initialize copy
    v_sql  :='INSERT INTO '|| v_ownertable ||'('|| v_pkey ||','|| v_rascolumn ||') VALUES ('|| v_seq ||'.nextval, sdo_geor.init('''|| v_rdt ||'''))';
    EXECUTE IMMEDIATE v_sql;
    v_debug:='Initialized RDT using '|| v_rdt ||'.';
    GOOM.RESPONSE(c_cmdname,v_debug);
    -- Get the new PID for the intialized image
    EXECUTE IMMEDIATE 'SELECT MAX('|| v_pkey ||') FROM '|| v_ownertable INTO n_maxid;
    -- Select output raster holder for update
    v_sql  :='SELECT '|| v_rascolumn ||' FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'='|| n_maxid ||' FOR UPDATE';
    EXECUTE IMMEDIATE v_sql INTO v_georOUT;
    -- Now change the parameters and perform the copy.
    SDO_GEOR.changeFormatCopy( v_georIn, v_parameters, v_georOut);
    v_debug:='New image created using '|| v_parameters ||'.';
    GOOM.RESPONSE( c_cmdname, v_debug);
    -- Insert the newly copied image.
    v_sql:='UPDATE '|| v_ownertable ||' SET '|| v_rascolumn ||' =:vgeorOut WHERE '|| v_pkey ||'='|| n_maxid;
    EXECUTE IMMEDIATE v_sql USING v_georOut;
    COMMIT;
    v_debug:='New image inserted using id= '|| n_maxid ||'.';
    GOOM.RESPONSE( c_cmdname, v_debug);
    -- Optionally update the table attributes.
    IF UPPER( v_update )='Y'  THEN
      v_sql       :='SELECT IMAGE_NAME FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'='|| i_imageid;
      EXECUTE IMMEDIATE v_sql INTO v_imagename;
      v_imagename := v_imagename||'_COPY';
      v_sql       :='UPDATE '|| v_ownertable ||' SET IMAGE_NAME ='''|| v_imagename ||''' WHERE '|| v_pkey ||'='|| n_maxid;
      EXECUTE IMMEDIATE v_sql;
      v_sql       :='UPDATE '|| v_ownertable ||' SET DATE_LOADED ='''|| SYSDATE ||''' WHERE '|| v_pkey ||'='|| n_maxid;
      EXECUTE IMMEDIATE v_sql;
      COMMIT;
      UpdateImageAttributes( v_ownertable, n_maxid);
    END IF;
    GOOM.RESPONSE( c_cmdname,'Image Copy Complete');
  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_parameters, v_debug, sqlerrm);
  END CopyImageWithParameters;
-- ---------------------------------------------------------------------------
  -- CopyImage copies the image referenced by the imageid to a new row in the indicated
  -- georaster table.
  -- Syntax: EXEC GDORASTER.UpdateImageAttributes(v_tablename, i_imageid);
  --  v_tablename : the georaster table to process
  --  i_imageid   : the image id to collect information from.
  --
  PROCEDURE CopyImage( v_tablename IN VARCHAR2, 
                         i_imageid IN INTEGER) IS
  --
    c_cmdname       CONSTANT VARCHAR2(9):='CopyImage';
    v_rascolumn     VARCHAR2(30);
    v_georIn        sdo_georaster;
    v_georOut       sdo_georaster;
    v_sql           VARCHAR2(512);
    v_parameters    VARCHAR2(255);
    v_debug         VARCHAR2(255):='Init';
    n_maxid         INTEGER;
    v_rdt           VARCHAR2(30);
    v_seq           VARCHAR2(30);
    v_imagename     VARCHAR2(64);
    v_pkey          VARCHAR2(30);
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30); 
  --   
  BEGIN
    -- Set parameters
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_owner      := GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    v_pkey       := GOOM.GetKeyCol( v_ownertable);
    v_rascolumn  := GetRasterColumn( v_ownertable);
    v_rdt        := GetTableRDT( v_ownertable);
    v_seq        := GOOM.GetSequenceName( v_ownertable);
    -- Set the storage parameters
    v_sql  :='SELECT '|| v_rascolumn ||' FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql INTO v_georIn;
    v_debug:='Retrieved image '|| v_imagename ||'.';
    GOOM.RESPONSE( c_cmdname, v_debug);
    -- Initialize copy
    v_sql  :='INSERT INTO '|| v_ownertable ||'('|| v_pkey ||','|| v_rascolumn ||') VALUES ('|| v_seq ||'.nextval, sdo_geor.init('''|| v_rdt ||'''))';
    EXECUTE IMMEDIATE v_sql;
    v_debug:='Initialized RDT using '|| v_rdt ||'.';
    GOOM.RESPONSE( c_cmdname, v_debug);
    -- Get the new PID for the intialized image
    EXECUTE IMMEDIATE 'SELECT MAX('|| v_pkey ||') FROM '|| v_tablename INTO n_maxid;
    -- Select output raster holder for update
    v_sql :='SELECT '|| v_rascolumn ||' FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'='|| n_maxid ||' FOR UPDATE';
    EXECUTE IMMEDIATE v_sql INTO v_georOUT;
    -- Now change the parameters and perform the copy.
    SDO_GEOR.Copy( v_georIn, v_georOut);
    -- Insert the newly copied image.
    v_sql :='UPDATE '|| v_ownertable ||' SET '|| v_rascolumn ||' =:vgeorOut WHERE '|| v_pkey ||'='|| n_maxid;
    EXECUTE IMMEDIATE v_sql USING v_georOut;
    COMMIT;
    -- Optionally update the table attributes.
    v_sql:='SELECT IMAGE_NAME FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql INTO v_imagename;
    v_imagename := v_imagename ||'_COPY';
    v_sql := 'UPDATE '|| v_ownertable ||' SET IMAGE_NAME ='''|| v_imagename ||''' WHERE '|| v_pkey ||'='|| n_maxid;
    EXECUTE IMMEDIATE v_sql;
    v_sql := 'UPDATE '|| v_ownertable ||' SET DATE_LOADED ='''|| SYSDATE ||''' WHERE '|| v_pkey ||'='|| n_maxid;
    EXECUTE IMMEDIATE v_sql;
    COMMIT;
    UpdateImageAttributes( v_ownertable, n_maxid);
    v_debug := 'Image id '|| i_imageid ||' copied to id= '|| n_maxid ||'.';
    GOOM.RESPONSE( c_cmdname, v_debug);
    GOOM.RESPONSE( c_cmdname,'Image Copy Complete');
  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_parameters, v_debug,sqlerrm);
  END CopyImage;
-- ---------------------------------------------------------------------------
  -- SetImagePyramid sets or alters the overview pyramids for an image 
  -- referenced by the imageid in the indicated georaster table.
  -- Syntax: EXEC GDORASTER.SetImagePyramid(v_tablename,i_imageid,i_level,v_method,v_update);
  --  v_tablename : the georaster table to process
  --  i_imageid   : the image id to collect information from.
  --  i_level      : maximum number of overviews, NULL (default) for optimal (calculated). 
  --  v_method     : Resampling: AVERAGE4(default), AVERAGE16, NN, CUBIC'
  --  v_update     : Update image attributes Y(default)/N 
  --
  PROCEDURE SetImagePyramid( v_tablename IN VARCHAR2,
                               i_imageid IN INTEGER, 
                                 i_level IN INTEGER DEFAULT NULL,
                                v_method IN VARCHAR2 DEFAULT 'AVERAGE4',
                                v_update IN VARCHAR2 DEFAULT 'Y') IS
    c_cmdname        CONSTANT VARCHAR2(15):='SetImagePyramid';
    c_operation      CONSTANT VARCHAR2(11):='ALTER IMAGE';
    v_rlevel         VARCHAR2(32):=NULL;
    v_sampling       VARCHAR2(32):=NULL;
    v_pyramidParams  VARCHAR2(255):=NULL;
    v_pkey           VARCHAR2(30);
    v_rascolumn      VARCHAR2(30);
    v_sql            VARCHAR2(255);
    v_debug          VARCHAR2(255);
    v_geor           SDO_GEORASTER;
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30); 
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_owner      := GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    v_pkey       := GOOM.GetKeyCol( v_ownertable);
    v_rascolumn  := GetRasterColumn( v_ownertable);
    --
    IF i_level IS NOT NULL THEN
      v_rlevel:='rlevel='|| i_level ||', ';
    END IF;
    IF v_method IN ('AVERAGE4','AVERAGE16','NN','BILINEAR','CUBIC') THEN
      v_sampling:='resampling='|| v_method;
    ELSE
      v_sampling:='resampling='|| c_resampling;
    END IF;
    v_pyramidParams:=RTRIM(LTRIM( v_rlevel || v_sampling));
    --
    v_sql:='SELECT '|| v_rascolumn ||' FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'='|| i_imageid ||' FOR UPDATE';
    EXECUTE IMMEDIATE v_sql INTO v_geor;
    v_debug:='Image id '|| i_imageid ||' selected for update';
    GOOM.Response( c_cmdname, v_debug);
    SDO_GEOR.generatePyramid( v_geor, v_pyramidParams);
    v_sql:='UPDATE '|| v_ownertable ||' SET '|| c_rascolumn ||' =:vgeor WHERE '|| v_pkey ||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql USING v_geor;
    COMMIT;
    v_debug:='Overview pyramid using '|| v_pyramidParams ||' complete.';
    GOOM.Response( c_cmdname, v_debug);
    IF UPPER( v_update)='Y'  THEN
      UpdateImageAttributes( v_ownertable, i_imageid);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_pyramidParams, v_debug,sqlerrm);
      ROLLBACK;
  END SetImagePyramid;
-- ---------------------------------------------------------------------------
  -- SetImageStatistics sets or alters the statistics for an image referenced
  -- by the imageid in the indicated georaster table.
  --  Syntax: EXEC GDORASTER.SetImageStatistics(v_tablename,i_imageid,n_min,n_max, 
  --                                            n_median,n_mode,n_std,i_layer);
  --  v_tablename : the georaster table to process
  --  i_imageid   : the image id to collect information from.  
  --  n_min,n_max  : min/max pixel values. 
  --  n_median     : Median value.
  --  n_mode       : Mode value.
  --  n_std        : Standard Dev value.
  --  i_layer      : Pyramid level to set. Default is 0, actual image.
  --
  PROCEDURE SetImageStatistics( v_tablename IN VARCHAR2, 
                                  i_imageid IN INTEGER,
                                      n_min IN NUMBER,
                                      n_max IN NUMBER,
                                     n_mean IN NUMBER,
                                   n_median IN NUMBER,
                                     n_mode IN NUMBER,
                                      n_std IN NUMBER,
                                    i_layer IN INTEGER DEFAULT 0) IS
  -- 
    c_cmdname      CONSTANT VARCHAR2(18):='SetImageStatistics';
    v_pkey           VARCHAR2(30);
    v_rascolumn      VARCHAR2(30);
    v_sql            VARCHAR2(512);
    v_debug          VARCHAR2(255);
    v_geor           SDO_GEORASTER;
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30); 
  BEGIN
    v_ownertable := GOOM.GetOwnerObject(v_tablename);
    v_owner      := GOOM.SplitOwnerObject(v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject(v_ownertable,'TABLE');
    --
    v_pkey       :=GOOM.GetKeyCol( v_ownertable );
    v_rascolumn  :=GetRasterColumn( v_ownertable );
    --
    v_sql:='SELECT '|| v_rascolumn ||' FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'='|| i_imageid ||' FOR UPDATE';
    EXECUTE IMMEDIATE v_sql INTO v_geor;
    v_debug:='Image id '|| i_imageid ||' selected for update';
    GOOM.Response( c_cmdname, v_debug);
    SDO_GEOR.setStatistics( v_geor, i_layer, SDO_NUMBER_ARRAY( n_min, n_max, n_mean, n_median, n_mode, n_std));
    v_sql:='UPDATE '|| v_ownertable||' SET '|| c_rascolumn||' =:vgeor WHERE '|| v_pkey||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql USING v_geor;
    COMMIT;
    v_debug:='Updated statistics for '|| v_ownertable ||'.'|| v_pkey ||'='|| i_imageid ||'.';
    GOOM.Response( c_cmdname, v_debug);
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_debug,sqlcode,sqlerrm);
      ROLLBACK;
  END SetImageStatistics;
-- ---------------------------------------------------------------------------
  -- ComputeImageStatistics computes the statistics for an image referenced
  -- by the imageid in the indicated georaster table.
  -- Syntax: EXEC GDORASTER.ComputeImageStatistics(v_tablename, i_imageid);
  --  v_tablename : the georaster table to process
  --  i_imageid   : the image id to collect information from.
  --  
  PROCEDURE ComputeImageStatistics( v_tablename IN VARCHAR2, i_imageid IN INTEGER) IS
  -- 
    c_cmdname        CONSTANT VARCHAR2(22):='ComputeImageStatistics';
    c_sampling       VARCHAR2(20) := 'samplingFactor=2';
    v_pkey           VARCHAR2(30);
    v_rascolumn      VARCHAR2(30);
    v_sql            VARCHAR2(512);
    v_debug          VARCHAR2(255);
    v_geor           SDO_GEORASTER;
    v_dummy          VARCHAR2(255);
    v_celldim        SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY(0,0);
    v_window         SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY(0,0,0,0);
    v_stats          SDO_NUMBER_ARRAY := SDO_NUMBER_ARRAY(0,0,0,0,0,0);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30); 
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_owner      := GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    v_pkey       := GOOM.GetKeyCol( v_ownertable );
    v_rascolumn  := GetRasterColumn( v_ownertable );
    --
    v_sql:='SELECT '|| v_rascolumn ||' FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'='|| i_imageid ||' FOR UPDATE';
    EXECUTE IMMEDIATE v_sql INTO v_geor;
    v_debug   := 'Start Stats...';
    v_celldim := SDO_GEOR.GetSpatialDimSizes( v_geor );
    v_window  := SDO_NUMBER_ARRAY(0, 0, v_celldim(1), v_celldim(2));
    v_dummy   := SDO_GEOR.GenerateStatistics( v_geor, c_sampling, v_window, 'TRUE', 0, 'FALSE');
    v_sql     := 'UPDATE '|| v_ownertable||' SET '|| c_rascolumn||' =:vgeor WHERE '|| v_pkey||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql USING v_geor;
    COMMIT;
    v_sql     :='SELECT SDO_GEOR.getStatistics('|| c_rascolumn ||' , 0) FROM '|| v_ownertable ||' WHERE '|| v_pkey||'='|| i_imageid;
    EXECUTE IMMEDIATE v_sql INTO v_stats;
    GOOM.Response( c_cmdname,'Updated statistics for '|| v_ownertable||'.'|| v_pkey ||'='|| i_imageid||'.');
    GOOM.Response('- MIN', v_stats(1));
    GOOM.Response('- MAX', v_stats(2));
    GOOM.Response('- MEAN', v_stats(3));
    GOOM.Response('- MEDIAN', v_stats(4));
    GOOM.Response('- MODE', v_stats(5));
    GOOM.Response('- STD', v_stats(6));
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_debug, sqlcode, sqlerrm);
      ROLLBACK;
  END ComputeImageStatistics;
-- ---------------------------------------------------------------------------
  -- SetFilePrivileges sets the required JAVA IO READ privileges on the specified image file 
  -- to both MDSYS and to PUBLIC.  This is required in order to load the file to the
  -- database.
  -- Syntax: EXEC GDORASTER.SetFilePrivileges(v_filename);
  --  v_filename : the file to grant read privileges on.
  --
  PROCEDURE SetFilePrivileges( v_filename IN VARCHAR2) IS
  c_cmdname         CONSTANT VARCHAR2(17) := 'SetFilePrivileges';
  c_feedback        CONSTANT VARCHAR2(40) := 'SYS:java.io.FilePermission granted: ';
  c_userA           CONSTANT VARCHAR2(30) := 'MDSYS';
  c_userB           CONSTANT VARCHAR2(30) := 'PUBLIC';
  c_priv            CONSTANT VARCHAR2(4)  := 'read';
  v_file            VARCHAR2(255);
  v_debug           VARCHAR2(255);
  BEGIN
    v_file  := SUBSTR(v_filename,INSTR(v_filename,'\',-1)+1);
    SYS.DBMS_JAVA.GRANT_PERMISSION( c_userA, 'SYS:java.io.FilePermission', v_filename, c_priv);
    v_debug := c_feedback || v_file ||' to '|| c_userA;
    GOOM.Response( c_cmdname, v_debug);
    SYS.DBMS_JAVA.GRANT_PERMISSION( c_userB, 'SYS:java.io.FilePermission', v_filename, c_priv);
    v_debug := c_feedback || v_file ||' to '|| c_userB;
    GOOM.Response( c_cmdname, v_debug);
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_filename, v_debug, sqlcode, sqlerrm);
  END SetFilePrivileges;
-- ---------------------------------------------------------------------------
  -- DBASetJAVAPolicy (DBA Only) sets the required JAVA IO Policy permissions to the 
  -- specified schema.  This is the schema that owns the georaster tables.  This should
  -- be done prior to any other operations.  It only needs to be done once.
  -- Syntax: EXEC GDORASTER.DBASetJAVAPolicy(v_schema);
  --  v_schema : the schema requiring the privileges.
  --
  PROCEDURE DBASetJAVAPolicy( v_schema IN VARCHAR2) IS
    c_cmdname       CONSTANT VARCHAR2(16) := 'DBASetJAVAPolicy';
    c_feedback      CONSTANT VARCHAR2(40) := 'SYS:java.io.FilePermission granted: ';
    v_user          VARCHAR2(30);
    v_host          VARCHAR2(30);
    v_sql           VARCHAR2(255);
  BEGIN
    v_user :=UPPER( v_schema );
    v_sql  :='SELECT HOST_NAME FROM v$instance';
    EXECUTE IMMEDIATE v_sql INTO v_host;
    SYS.DBMS_JAVA.GRANT_POLICY_PERMISSION( v_user,'SYS','java.io.FilePermission','*');
    SYS.DBMS_JAVA.GRANT_PERMISSION( v_user, 'java.net.SocketPermission', v_host,'resolve');
    GOOM.Response( c_cmdname, c_feedback || v_user);
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_schema, 'None', sqlcode, sqlerrm);
  END DBASetJAVAPolicy;
-- ---------------------------------------------------------------------------
  FUNCTION GetRasterColumn( v_tablename IN VARCHAR2) RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(15) := 'GetRasterColumn';
    c_feedbackA     CONSTANT VARCHAR2(40) := 'Multiple georaster columns found in: ';
    c_feedbackB     CONSTANT VARCHAR2(40) := ', first georaster returned.';
    --
    v_sql           VARCHAR2(200);
    v_rascol        VARCHAR2(30);
    v_cur           PLS_INTEGER;
    v_Dum           PLS_INTEGER;
    v_count         PLS_INTEGER;
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_owner         VARCHAR2(30);
    v_table         VARCHAR2(30); 
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_owner      := GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    v_sql        := 'SELECT COUNT(*) FROM ALL_TAB_COLS WHERE OWNER=:vowner AND DATA_TYPE=''SDO_GEORASTER'' AND TABLE_NAME=:vtablename';
    EXECUTE IMMEDIATE (v_sql) INTO v_count USING v_owner, v_table;
    v_sql := 'SELECT COLUMN_NAME FROM ALL_TAB_COLS WHERE OWNER='''|| v_owner ||''' AND DATA_TYPE=''SDO_GEORASTER'' AND TABLE_NAME=''' || v_table || '''';
    v_cur := SYS.DBMS_SQL.OPEN_CURSOR;
    SYS.DBMS_SQL.PARSE( v_cur, v_sql, SYS.DBMS_SQL.NATIVE);
    SYS.DBMS_SQL.DEFINE_COLUMN( v_cur, 1, v_rascol, 30);
    v_Dum := SYS.DBMS_SQL.EXECUTE( v_cur );
    v_Dum := SYS.DBMS_SQL.FETCH_ROWS( v_cur );
    SYS.DBMS_SQL.COLUMN_VALUE( v_cur, 1, v_rascol);
    SYS.DBMS_SQL.CLOSE_CURSOR( v_cur );
    IF v_count > 1 THEN
	  GOOM.Response( c_cmdname || c_msgWarning, c_feedbackA || v_ownertable || c_feedbackB);
    END IF;
    RETURN v_rascol; --Return the name of the georaster column or NULL.
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR( c_cmdname, v_tablename, v_sql, SQLCODE, SQLERRM);
      RETURN NULL;
  END GetRasterColumn;
-- ---------------------------------------------------------------------------
  -- GetRasterExtentName returns the raster extent name for use in assigning Oracle 
  -- metadata in USER_SDO_GEOM_METADATA.  The extent name uses the georaster column
  -- in the specified table and combines it with SPATIALEXTENT in the form:
  -- raster_column_name.SPATIALEXTENT
  -- Syntax: v_extentname:= GDORASTER.GetRasterExtentName(v_tablename);
  --  v_tablename : the georaster table to process
  --
  FUNCTION GetRasterExtentName( v_tablename IN VARCHAR2) RETURN VARCHAR2 IS
      c_cmdname    CONSTANT VARCHAR2(19) := 'GetRasterExtentName';
      --
      v_invalid    EXCEPTION;
      v_rascol     VARCHAR2(32);
      v_rasextent  VARCHAR2(61);
    BEGIN
      v_rascol := GetRasterColumn( v_tablename );
      IF v_rascol IS NULL THEN
        raise v_invalid;
      ELSE
        v_rasextent := v_rascol||'.SPATIALEXTENT';
      END IF;
      RETURN v_rasextent; --Return the name of the raster spatial extents.
    EXCEPTION
      WHEN v_invalid THEN
        GOOM.REPORT_ERROR( c_cmdname, v_tablename, v_rascol, SQLCODE, SQLERRM);
      WHEN OTHERS THEN
        GOOM.REPORT_ERROR( c_cmdname, v_tablename, v_rascol, SQLCODE, SQLERRM);
  END GetRasterExtentName;
-- ---------------------------------------------------------------------------
  -- GetImageSRID returns the SRID used by georaster stored in the specified table.
  -- Syntax: i_srid := GDORASTER.GetImageSRID(v_tablename);
  --  v_tablename : the georaster table to process
  --
  FUNCTION GetImageSRID( v_tablename IN VARCHAR2) RETURN INTEGER IS
      c_cmdname    CONSTANT VARCHAR2(12) := 'GetImageSRID';
      v_rasextent  VARCHAR2(64);
      v_sql        VARCHAR2(512);
      i_srid       INTEGER;
      -- added for owner.table support
      v_ownertable    VARCHAR2(61);
    BEGIN
      v_ownertable := GOOM.GetOwnerObject( v_tablename);
      v_rasextent  := GetRasterExtentName( v_ownertable);
      v_sql        := 'SELECT NVL(A.'|| v_rasextent ||'.SDO_SRID,0) FROM '|| v_ownertable ||' A WHERE ROWNUM=1';
      EXECUTE IMMEDIATE v_sql INTO i_srid;
      RETURN i_srid; --Return the srid of the raster spatial extents.
    EXCEPTION
      WHEN OTHERS THEN
        RETURN 0;
  END GetImageSRID;
-- ---------------------------------------------------------------------------
  -- GetImageBands returns the band information for the image referenced
  -- by the imageid in the indicated georaster table.
  -- Syntax: v_imagebands := GDORASTER.GetImageBands(v_tablename, i_imageid);
  --  v_tablename : the georaster table to process
  --  i_imageid   : the image id to collect information from.
  --  
  FUNCTION GetImageBands ( v_tablename IN VARCHAR2, i_imageid IN INTEGER) RETURN VARCHAR2 IS
    c_cmdname          CONSTANT VARCHAR2(13):='GetImageBands';
    v_sql              VARCHAR2(512);
    v_georaster        VARCHAR2(30);
    v_band             INTEGER;
    v_bands            VARCHAR2(2);
    v_pkey             VARCHAR2(30);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
  begin
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_georaster  := GDORASTER.GetRasterColumn( v_ownertable);
    v_pkey       := GOOM.GetKeyCol( v_ownertable);
    v_sql        := 'SELECT sdo_geor.getBandDimSize('|| v_georaster ||') from '|| v_ownertable ||' where '|| v_pkey ||'=:pid';
    EXECUTE IMMEDIATE v_sql INTO v_band USING i_imageid;
    v_bands:=TO_CHAR( v_band );
    RETURN v_bands;
  EXCEPTION
    WHEN OTHERS THEN
      RETURN 0;
  END GetImageBands;
-- ---------------------------------------------------------------------------
  FUNCTION GetImageBlocking ( v_tablename IN VARCHAR2, i_imageid IN INTEGER) RETURN VARCHAR2 IS
    c_cmdname          CONSTANT VARCHAR2(16):='GetImageBlocking';
    v_sql              VARCHAR2(512);
    v_bands            VARCHAR2(2);
    v_georaster        VARCHAR2(30);
    v_block            MDSYS.SDO_NUMBER_ARRAY:=NULL;
    v_blocking         VARCHAR2(32);
    v_pkey             VARCHAR2(30);
    v_ownertable       VARCHAR2(61);
  BEGIN
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_georaster  := GDORASTER.GetRasterColumn( v_ownertable);
    v_pkey       := GOOM.GetKeyCol( v_ownertable);
    v_bands      := GetImageBands( v_ownertable, i_imageid);
    v_sql        := 'SELECT sdo_geor.getBlockSize('|| v_georaster ||') from '|| v_ownertable ||' where '|| v_pkey ||'=:pid';
    EXECUTE IMMEDIATE v_sql INTO v_block USING i_imageid;
    v_blocking:='('|| v_block(1) ||','|| v_block(2) ||','|| v_bands ||')';
    RETURN v_blocking;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_blocking,sqlcode,sqlerrm);
       RETURN '(0,0,0)';
  END GetImageBlocking;
-- ---------------------------------------------------------------------------
  -- GetImageInterleave returns the interleave information for the image referenced
  -- by the imageid in the indicated georaster table.
  -- Syntax: v_interleave := GDORASTER.GetImageInterleave(v_tablename, i_imageid);
  --  v_tablename : the georaster table to process
  --  i_imageid   : the image id to collect information from.
  -- 
  FUNCTION GetImageInterleave ( v_tablename IN VARCHAR2, i_imageid IN INTEGER) RETURN VARCHAR2 IS
    c_cmdname           CONSTANT VARCHAR2(18):='GetImageInterleave';
    v_sql               VARCHAR2(512);
    v_interleave        VARCHAR2(3);
    v_rascolumn         VARCHAR2(30);
    v_pkey              VARCHAR2(30);
    v_ownertable        VARCHAR2(61);
  begin
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_rascolumn  := GDORASTER.GetRasterColumn( v_ownertable);
    v_pkey       := GOOM.GetKeyCol( v_ownertable);
    v_sql        := 'SELECT SUBSTR(SDO_GEOR.getInterleavingType('|| v_rascolumn||'),1,8) FROM '|| v_ownertable||' WHERE '|| v_pkey||'=:vpid';
    EXECUTE IMMEDIATE v_sql INTO v_interleave USING i_imageid;
    RETURN v_interleave;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, v_interleave,sqlcode,sqlerrm);
  END GetImageInterleave;
-- ---------------------------------------------------------------------------
  -- GetMaxImageOverview returns the maximum pyramid level used by the image referenced
  -- by the imageid in the indicated georaster table.
  -- Syntax: v_maxOverview := GDORASTER.GetMaxImageOverview(v_tablename, i_imageid);
  --  v_tablename : the georaster table to process
  --  i_imageid   : the image id to collect information from.
  --
  FUNCTION GetMaxImageOverview ( v_tablename IN VARCHAR2, i_imageid IN INTEGER) RETURN INTEGER IS
    c_cmdname          CONSTANT VARCHAR2(19):='GetMaxImageOverview';
    v_sql              VARCHAR2(512);
    v_rascolumn        VARCHAR2(30);
    v_pkey             VARCHAR2(30);
    n_maxov            PLS_INTEGER;
    v_ownertable        VARCHAR2(61);
  begin
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_rascolumn  :=GDORASTER.GetRasterColumn( v_ownertable);
    v_pkey       :=GOOM.GetKeyCol( v_tablename);
    v_sql        :='SELECT SDO_GEOR.getPyramidMaxLevel('|| v_rascolumn||') FROM '|| v_ownertable||' WHERE '|| v_pkey||'=:vpid';
    EXECUTE IMMEDIATE v_sql INTO n_maxov USING i_imageid;
    RETURN n_maxov;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, n_maxov,sqlcode,sqlerrm);
      RETURN 0;
  END GetMaxImageOverview;
-- ---------------------------------------------------------------------------
  -- GetImageCompression returns the image compression level used by the image referenced
  -- by the imageid in the indicated georaster table.
  -- Syntax: v_compression := GDORASTER.GetImageCompression(v_tablename, i_imageid);
  --  v_tablename : the georaster table to process
  --  i_imageid   : the image id to collect information from.
  --
  FUNCTION GetImageCompression( v_tablename IN VARCHAR2, i_imageid IN INTEGER) RETURN VARCHAR2 IS
    c_cmdname          CONSTANT VARCHAR2(19):='GetImageCompression';
    v_sql              VARCHAR2(512);
    v_rascolumn        VARCHAR2(30);
    v_compress         VARCHAR2(8);
    v_pkey             VARCHAR2(30);
    n_maxov            PLS_INTEGER;
    v_ownertable        VARCHAR2(61);
  begin
    v_ownertable := GOOM.GetOwnerObject( v_tablename);
    v_rascolumn  := GDORASTER.GetRasterColumn( v_ownertable);
    v_pkey       := GOOM.GetKeyCol( v_ownertable);
    v_sql        := 'SELECT SUBSTR(SDO_GEOR.getCompressionType('|| v_rascolumn ||'),1,8) FROM '|| v_ownertable ||' WHERE '|| v_pkey ||'=:vpid';
    EXECUTE IMMEDIATE v_sql INTO v_compress USING i_imageid;
    RETURN v_compress;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_sql, n_maxov,sqlcode,sqlerrm);
      RETURN 'NONE';
  END GetImageCompression;
-- ---------------------------------------------------------------------------
  -- ImageStorageParams returns storage parameter string to use in import, alter, and copy.
  -- Syntax: v_storageParams := GDORASTER.ImageStorageParams(v_blksize,v_interleave,v_compress,i_quality)
  --  v_blocking   : Blocking factor in the firm (n,n,b) eg. (256,256,3)  
  --  v_interleave : Interleaving value - BSQ, BIP, BIL'
  --  v_compression: Compression value - JPEG-B, JPEG-F, DEFLATE, NONE
  --  i_quality    : Compression quality (JPEG ONLY) 0-100 90 is default.
  -- 
  FUNCTION ImageStorageParams ( v_blksize IN VARCHAR2 DEFAULT NULL, 
                             v_interleave IN VARCHAR2 DEFAULT NULL,
                               v_compress IN VARCHAR2 DEFAULT NULL,
                                i_quality IN INTEGER  DEFAULT NULL,
                                   i_srid IN INTEGER  DEFAULT NULL) 
           RETURN VARCHAR2 IS
    c_cmdname          CONSTANT VARCHAR2(18):='ImageStorageParams';
    v_blockparm        VARCHAR2(32):=NULL;
    v_ileaveparm       VARCHAR2(32):=NULL;
    v_compparm         VARCHAR2(32):=NULL;
    v_georef           VARCHAR2(64):=NULL;
    v_seperator        VARCHAR2(1) :=' ';
    v_storParameters   VARCHAR2(256);
    i_qual             PLS_INTEGER;
  begin
    IF v_blksize IS NOT NULL THEN
      v_blockparm:='blocksize=' || UPPER( v_blksize ) || v_seperator;
    END IF;
    IF v_interleave IN ('BIP','BIL','BSQ') THEN
      v_ileaveparm:='interleaving=' || UPPER( v_interleave ) ||  v_seperator;
    END IF;
    IF v_compress IN ('DEFLATE','NONE') THEN
      v_compparm:='compression=' || UPPER( v_compress ) || v_seperator ;
    ELSIF v_compress IN ('JPEG-B','JPEG-F') THEN
      IF i_quality IS NULL THEN
        i_qual := c_DefaultQuality;
      ELSE 
        i_qual := i_quality;
      END IF;
      v_compparm:='compression=' || UPPER( v_compress ) || ' quality=' || i_qual || v_seperator;
    END IF;
    IF Version11 and i_srid is not NULL THEN
      v_georef:='raster=TRUE spatialExtent=TRUE srid=' || i_srid || v_seperator;
    END IF;
      v_storParameters:=RTRIM(LTRIM( v_blockparm || v_ileaveparm || v_compparm || v_georef));
    RETURN v_storParameters;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR ( c_cmdname, v_blksize, v_storParameters, sqlcode, sqlerrm);
      RETURN NULL;
  END ImageStorageParams;
-- ---------------------------------------------------------------------------
  -- GetTableRDT returns the name of the RDT table for the specific raster table if it exists
  -- otherwise it will create and initialize the RDT table before returning the name.
  -- Syntax: v_RDTtable:=gdoraster.GetTableRDT(v_tablename);
  --  v_tablename : the georaster table to process
  --
  FUNCTION GetTableRDT( v_tablename IN VARCHAR2) RETURN VARCHAR2 IS
    c_cmdname    CONSTANT VARCHAR2(11) := 'GetTableRDT';
    v_rdt        VARCHAR2(30);
    v_sql        VARCHAR2(512);
    v_count      PLS_INTEGER;
    v_size       NUMBER(10,2);
    -- added for owner.table support
    v_ownertable  VARCHAR2(61);
    v_owner       VARCHAR2(30);
    v_table       VARCHAR2(30);
  BEGIN
    v_ownertable  :=GOOM.GetOwnerObject( v_tablename);
    v_owner       :=GOOM.SplitOwnerObject( v_ownertable,'OWNER');
    v_table       :=GOOM.SplitOwnerObject( v_ownertable,'TABLE');
    -- Does the RDT table already exist for this table?
    v_sql:='SELECT COUNT(1) FROM ALL_SDO_GEOR_SYSDATA WHERE OWNER=:vowner AND TABLE_NAME=:vtab';
    EXECUTE IMMEDIATE v_sql INTO v_count USING v_owner, v_table;
    IF v_count>0 THEN -- If it does exist then get the current RDT.
      v_sql:='SELECT MAX(RDT_TABLE_NAME) FROM ALL_SDO_GEOR_SYSDATA WHERE OWNER=:vowner AND TABLE_NAME LIKE :vtab';
      EXECUTE IMMEDIATE v_sql INTO v_rdt USING v_owner, v_table;
      v_size := GOOM.GetTableSize( v_owner ||'.'|| v_rdt);     -- Get the size if the current RDT table.
      IF v_size > c_maxrdtsize THEN    -- If bigger than default size then, 
        v_rdt := CreateRasterRDT( v_owner );        -- automatically create a new one.
      END IF;
    ELSE -- None exists so create one.
      v_rdt:=CreateRasterRDT( v_owner );
    END IF;
    RETURN v_rdt; -- Return the RDT tablename.
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR( c_cmdname, v_tablename, v_rdt, SQLCODE, SQLERRM);
  END GetTableRDT;
-- ---------------------------------------------------------------------------
  -- CreateRasterRDT creates and returns the name a unique raster storage object in the
  -- specified schema.  The object can then be associated with a new georaster table.
  -- Syntax: v_RDTtable:=gdoraster.CreateRasterRDT(v_schema);
  --  v_schema : the schema that will own the georaster table and associated RDT object.
  --
  FUNCTION CreateRasterRDT( v_owner IN VARCHAR2 DEFAULT USER) RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(15) := 'CreateRasterRDT';
    c_feedback      CONSTANT VARCHAR2(33) := 'Raster Data Table (RDT) created: ';
    --
    v_sql           VARCHAR2(512);
    v_rdt           VARCHAR2(30);
    v_seg           VARCHAR2(30);
    v_rasobj        SDO_GEORASTER;
  BEGIN
    SELECT MDSYS.SDO_GEOR.INIT INTO v_rasobj FROM DUAL; -- Returns a blank SDO_RASTER object and its unique name.
    v_rdt := v_rasobj.RASTERDATATABLE;                    -- Extract the name and use when creating the raster table.
    v_seg := v_rdt||'_LOB';
    IF UPPER( c_uselobspace ) = 'YES' THEN
      IF NOT Version11 THEN
           v_sql:='CREATE TABLE '|| v_owner||'.'|| v_rdt||' OF MDSYS.SDO_RASTER(PRIMARY KEY (RASTERID, PYRAMIDLEVEL, BANDBLOCKNUMBER, ROWBLOCKNUMBER, COLUMNBLOCKNUMBER))
              TABLESPACE '|| c_rdttablespace||' NOLOGGING LOB(RASTERBLOCK) STORE AS '|| v_seg||' (TABLESPACE '|| c_lobtablespace||' CHUNK '|| c_rdtchunk||' CACHE READS NOLOGGING PCTVERSION 0 STORAGE (PCTINCREASE 0))';
      ELSE
           v_sql:='CREATE TABLE '|| v_owner||'.'|| v_rdt||' OF MDSYS.SDO_RASTER(PRIMARY KEY (RASTERID, PYRAMIDLEVEL, BANDBLOCKNUMBER, ROWBLOCKNUMBER, COLUMNBLOCKNUMBER))
              TABLESPACE '|| c_rdttablespace||' NOLOGGING LOB(RASTERBLOCK) STORE AS SECUREFILE '|| v_seg||' (TABLESPACE '|| c_lobtablespace||' )';
      END IF;
    ELSE
      v_sql:='CREATE TABLE '|| v_owner||'.'|| v_rdt||' OF MDSYS.SDO_RASTER(PRIMARY KEY (RASTERID, PYRAMIDLEVEL, BANDBLOCKNUMBER, ROWBLOCKNUMBER, COLUMNBLOCKNUMBER))
              TABLESPACE '|| c_rdttablespace||' NOLOGGING';
    END IF;
    EXECUTE IMMEDIATE v_sql; -- Create the table defined above.
    GOOM.RESPONSE( c_cmdname, c_feedback|| v_rdt);
    RETURN v_rdt;            -- Return the name of the Raster table.
    --
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR( c_cmdname, v_rdt, v_sql, SQLCODE, SQLERRM);
  END CreateRasterRDT;
-- ---------------------------------------------------------------------------
  -- CreateStatisticsAll creates image statistic information for all georaster images 
  -- in the specified schema. 
  -- Syntax: EXEC GDORASTER.CreateStatisticsAll(v_schema, i_sample, b_overwrite);
  --  v_schema : the schema that contains the image tables, default is current USER.
  --  i_sample : smapling value to use, default is 1000  (first 1000 rows).
  --  b_overwrite : Over write existing stats, default is TRUE.
  --
  PROCEDURE CreateStatisticsAll( v_owner IN VARCHAR2 DEFAULT USER, i_sample IN PLS_INTEGER DEFAULT 1000, b_overwrite IN BOOLEAN DEFAULT TRUE) IS
    c_cmdname       CONSTANT VARCHAR2(25) := 'CreateStatisticsAll';
    c_feedback      CONSTANT VARCHAR2(30) := 'Process statistics for RID: ';
    v_RasterObj     GetRasterIDs%ROWTYPE;
  BEGIN
    GOOM.Response( c_cmdname, c_msgStart || v_owner,28);
    IF GetRasterIDs%ISOPEN THEN
      CLOSE GetRasterIDs;
    END IF;
    OPEN GetRasterIDs( v_owner );
    FETCH GetRasterIDs INTO v_RasterObj;
    WHILE GetRasterIDs%FOUND LOOP
      GOOM.RESPONSE( c_cmdname, c_feedback || v_RasterObj.RASTER_ID);
      CreateStatistics( v_RasterObj.RASTER_ID, i_sample, b_overwrite);
      FETCH GetRasterIDs INTO v_RasterObj;
    END LOOP;
    CLOSE GetRasterIDs;
    GOOM.Response( c_cmdname, c_msgComplete || v_owner,28);
   EXCEPTION WHEN OTHERS THEN
    IF GetRasterIDs%ISOPEN THEN
      CLOSE GetRasterIDs;
    END IF;
    RAISE;
  END CreateStatisticsAll;
-- ---------------------------------------------------------------------------
  -- CreateStatistics creates image statistic information for the specified image id.
  -- Syntax: EXEC GDORASTER.CreateStatistics(i_rasterid, i_sample, b_overwrite);
  --  i_rasterid : the unique raster id that contains the image.
  --  i_sample : smapling value to use, default is 1000  (first 1000 rows).
  --  b_overwrite : Over write existing stats, default is TRUE.
  --
  PROCEDURE CreateStatistics( i_rasterid IN PLS_INTEGER, i_sample IN PLS_INTEGER DEFAULT 1000, b_overwrite IN BOOLEAN DEFAULT FALSE) IS
  TYPE GeorasterType IS RECORD (GEORASTER SDO_GEORASTER);
  c_cmdname             CONSTANT VARCHAR2(20) :='CreateStatistics';
  v_RasterObj           GetRasterObj%ROWTYPE;
  v_sql                 VARCHAR2(1024);
  v_georaster           SDO_GEORASTER;
  v_REFCURSOR           SYS_REFCURSOR;
  v_RC_ROW              GeorasterType;
  i_SampleCount         PLS_INTEGER;
  i_StdDev              PLS_INTEGER := 3;
  i_RedBand             PLS_INTEGER := 1;
  i_GreenBand           PLS_INTEGER := 2;
  i_BlueBand            PLS_INTEGER := 3;
  i_MaxRow              PLS_INTEGER;
  i_MaxColumn           PLS_INTEGER;
  i_MaxBand             PLS_INTEGER;
  i_Band                PLS_INTEGER;
  i_Index               PLS_INTEGER;
  i_COUNT               PLS_INTEGER;
  v_XMLNameSpace        VARCHAR2(256) := 'xmlns="http://xmlns.oracle.com/spatial/georaster"';
  v_result              SYS.DBMS_STAT_FUNCS.summaryType;
  v_owner               VARCHAR2(30);
  v_debug               VARCHAR2(50) := 'INIT';
  --
  BEGIN
    IF GetRasterObj%ISOPEN THEN
      CLOSE GetRasterObj;
    END IF;
    v_debug := 'GetRasterObj';
    OPEN GetRasterObj( i_rasterid );
    FETCH GetRasterObj INTO v_RasterObj;
    WHILE GetRasterObj%FOUND LOOP
      v_owner := v_RasterObj.OWNER;
      v_sql   := 'SELECT a.'|| v_RasterObj.COLUMN_NAME ||' AS GEORASTER FROM '|| v_RasterObj.TABLE_NAME ||' a WHERE a.'|| v_RasterObj.COLUMN_NAME ||'.RASTERID='|| i_rasterid;
      OPEN v_REFCURSOR FOR v_sql;
      i_COUNT := 0;
      FETCH v_REFCURSOR INTO v_RC_ROW;
      WHILE v_REFCURSOR%FOUND LOOP
        IF v_RC_ROW.GEORASTER.RasterDataTable IS NOT NULL THEN
          i_COUNT := i_COUNT + 1;
          v_georaster := v_RC_ROW.GEORASTER;
          IF ( b_overwrite OR SDO_GEOR.getStatistics( v_georaster, 0) IS NULL) THEN
            -- Perform Statistics on Image --
            -- Truncate the Temporary GDOSYS.GDORASTER_STATS_TMP --
      	    DELETE FROM GDOSYS.GDORASTER_STATS_TMP;
            COMMIT;
      	    -- Pull the XML Values for Image Dimensions, Must include NAMESPACE --
            i_MaxRow := v_georaster.metadata.extract('//dimensionSize[@type="ROW"]/size/text()',v_XMLNameSpace).getNumberVal();
      	    i_MaxColumn := v_georaster.metadata.extract('//dimensionSize[@type="COLUMN"]/size/text()',v_XMLNameSpace).getNumberVal();
      	    -- Check to see if there is a third dimension --
      	    IF v_georaster.metadata.existsNode('//dimensionSize[@type="BAND"]/size/text()',v_XMLNameSpace)=1 THEN
      		  i_MaxBand := v_georaster.metadata.extract('//dimensionSize[@type="BAND"]/size/text()',v_XMLNameSpace).getNumberVal();
      	    ELSE
      		  i_MaxBand := 1;
      	    END IF;
      	    -- Get the Default RGB Layers --
      	    i_RedBand   := sdo_geor.getDefaultRed( v_georaster);
      	    i_GreenBand := sdo_geor.getDefaultGreen( v_georaster);
      	    i_BlueBand  := sdo_geor.getDefaultBlue( v_georaster);
            v_debug     := 'Set Bands using SDO_GEOR';
      	    -- Take a sample set --
      	    -- If More than 4 Sample on the Default RGB bands --
      	    IF i_MaxBand > 3 THEN
      		  GOOM.Response( c_cmdname,'Sampling only RGB Layers: '|| i_RedBand ||','|| i_GreenBand ||','|| i_BlueBand );
              v_debug     := 'Looping SDO_GEOR.getCellValue MB>3';
      		  FOR i_SampleCount IN 1..i_sample LOOP
      			INSERT INTO GDOSYS.GDORASTER_STATS_TMP VALUES (SDO_GEOR.getCellValue( v_georaster,0,FLOOR(DBMS_RANDOM.value(0, i_MaxRow-0.0001)),FLOOR(DBMS_RANDOM.value(0, i_MaxColumn-0.0001)), i_RedBand - 1));
      			INSERT INTO GDOSYS.GDORASTER_STATS_TMP VALUES (SDO_GEOR.getCellValue( v_georaster,0,FLOOR(DBMS_RANDOM.value(0, i_MaxRow-0.0001)),FLOOR(DBMS_RANDOM.value(0, i_MaxColumn-0.0001)), i_GreenBand - 1));
      			INSERT INTO GDOSYS.GDORASTER_STATS_TMP VALUES (SDO_GEOR.getCellValue( v_georaster,0,FLOOR(DBMS_RANDOM.value(0, i_MaxRow-0.0001)),FLOOR(DBMS_RANDOM.value(0, i_MaxColumn-0.0001)), i_BlueBand - 1));
      		  END LOOP;
              v_debug     := 'End SDO_GEOR.getCellValue';
      	    ELSE
      		  GOOM.Response( c_cmdname,'Sampling all ['|| i_MaxBand ||'] bands');
              v_debug     := 'Looping SDO_GEOR.getCellValue MB<=3';
      		  FOR i_Band IN 0..i_MaxBand-1 LOOP
    			  FOR i_SampleCount IN 1..i_sample LOOP
    			    INSERT INTO GDOSYS.GDORASTER_STATS_TMP VALUES (SDO_GEOR.getCellValue( v_georaster,0,FLOOR(DBMS_RANDOM.value(0, i_MaxRow-0.0001)),FLOOR(DBMS_RANDOM.value(0, i_MaxColumn-0.0001)), i_Band));
    			  END LOOP;
      		  END LOOP;
              v_debug     := 'INSERTS - GDORASTER_STATS_TMP';
      	    END IF;   
      	    -- Perform Statistics --
      	    -- Alternatively I could have used MIN(VALUE), MAX(VALUE), AVG(VALUE), MEDIAN(VALUE), STATS_MODE(VALUE), STDDEV(VALUE) –-
      	    v_debug     := 'Start Summary Stats';
            DBMS_STAT_FUNCS.SUMMARY('GDOSYS','GDORASTER_STATS_TMP','VALUE', i_StdDev, v_result);
      	    v_debug     := 'End Summary Stats';
      	    GOOM.RESPONSE(c_cmdname,'MIN:   '|| v_result.MIN);
      	    GOOM.RESPONSE(c_cmdname,'MAX:   '|| v_result.MAX);
      	    GOOM.RESPONSE(c_cmdname,'MEAN:  '|| v_result.MEAN);
      	    GOOM.RESPONSE(c_cmdname,'MEDIAN:'|| v_result.MEDIAN);
      	    FOR i_Index IN v_result.cmode.FIRST..v_result.cmode.LAST LOOP
      		  GOOM.RESPONSE(c_cmdname,'MODE[' || i_Index || ']: '|| v_result.cmode( i_Index ));
      	    END LOOP;
      	    GOOM.RESPONSE(c_cmdname,'STDDEV: '|| v_result.STDDEV);
      	    -- Set the Statistics --
            v_debug     := 'Entering SDO_GEOR.SETSTATISTICS';
      	    SDO_GEOR.SETSTATISTICS(v_georaster, 0, SDO_NUMBER_ARRAY(v_result.MIN,v_result.MAX,FLOOR(v_result.MEAN),v_result.MEDIAN,v_result.CMODE(1),FLOOR(v_result.STDDEV)));
      	    EXECUTE IMMEDIATE 'UPDATE '|| v_owner ||'.'|| v_RasterObj.TABLE_NAME ||' a SET a.'|| v_RasterObj.COLUMN_NAME ||'=:v_georaster WHERE a.'|| v_RasterObj.COLUMN_NAME ||'.RASTERID='|| i_rasterid USING v_georaster;
    	    v_debug     := 'Finished SDO_GEOR.SETSTATISTICS';
          ELSE
    	    GOOM.RESPONSE(c_cmdname,'Statistics already exist for: '|| v_georaster.RASTERID);
    	  END IF; -- CHECK OVERWRITE
        END IF;   -- CHECK GEORASTER AVAILABLE
        FETCH v_REFCURSOR INTO v_RC_ROW;
      END LOOP;
      FETCH GetRasterObj INTO v_RasterObj;
      CLOSE v_REFCURSOR;
      COMMIT;
    END LOOP;
    CLOSE GetRasterObj;
    EXCEPTION WHEN OTHERS THEN
      GOOM.REPORT_ERROR( c_cmdname, 'RID:'|| i_rasterid, v_debug, v_sql, SQLERRM);
      IF GetRasterObj%ISOPEN THEN
        CLOSE GetRasterObj;
      END IF;
      IF v_REFCURSOR%ISOPEN THEN
        CLOSE v_REFCURSOR;
      END IF;
      RAISE;
  END CreateStatistics;
-- ---------------------------------------------------------------------------
END GDORASTER;
/
SET SERVEROUTPUT ON;
SHOW ERRORS
PROMPT **..............................................................**;
PROMPT **           IMPORTANT INFORMATION PLEASE READ THIS!            **;
PROMPT **..............................................................**;
PROMPT ** NOTE: Any errors occurring above may leave package unusable. **;
PROMPT ** NOTE: This package requires Oracle Spatial 11G or later.     **;
SET TERMOUT OFF
GRANT EXECUTE ON GDOSYS.GDORASTER TO PUBLIC;
SET TERMOUT ON
PROMPT ** NOTE: Execute on GDORASTER Package Granted to PUBLIC.        **;
SET TERMOUT OFF
DROP PUBLIC SYNONYM GDORASTER;
CREATE PUBLIC SYNONYM GDORASTER FOR GDOSYS.GDORASTER;
SET TERMOUT ON
PROMPT ** NOTE: Public synonym GDORASTER created.                      **;
PROMPT **..............................................................**;
PROMPT ** To turn on package messages, enter: SET SERVEROUTPUT ON      **;  
PROMPT ** For version information, enter:     EXEC GDORASTER.VERSION   **;
PROMPT ** For Online Help, enter:             EXEC GDORASTER.HELPME    **;
PROMPT ** ***************   Installation completed.  ***************** **;
PROMPT ******************************************************************;
EXEC GDORASTER.VERSION;
-- ----------------------------------------------------------------------------------------