SET SERVEROUTPUT ON
-- ------------------------------------------------------------------------------------------------------------------
-- @GOOM_PKG
-- GeoMedia Oracle Object Model (GOOM) Package
-- Oracle Object Model Procedures and Functions for DBA's managing data for GeoMedia Applications. 
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
-- This package supports Oracle 11G and 12C, it does not support earlier versions of Oracle.  Run this script as 
-- system DBA after the creation of the GDOSYS metadata schema.  The GOOM package is installed into GDOSYS and is 
-- accessed via a GOOM Public synonym.  Any user account that makes use of this package must be explicitly granted 
-- the following privileges in order to manipulate spatial indexes:
--   GRANT CREATE TABLE TO <user>; 
--   GRANT CREATE SEQUENCE TO <user>;
--
-- GOOM also uses DBMS_SQL to process internal cursors so you need the folowing privileges: 
--   GRANT EXECUTE ON SYS.DBMS_SQL TO PUBLIC; -- or 
--   GRANT EXECUTE ON SYS.DBMS_SQL TO <user>;
--
-- Oracle 12c has tightened security on executing procedures owned by other users.  GDOSYS needs to 
-- inherit the privileges of any user that is running GOOM and this includes any DBA user like SYSTEM.
-- For example, if a DBA account called SYSTEM needs to run GDOSYS.GOOM, you need to do the following:
--    GRANT INHERIT ANY PRIVILEGES TO SYSTEM;
--    GRANT INHERIT PRIVILEGES ON USER SYSTEM TO GDOSYS;
-- The later allows GOOM to be run using the privileges of the DBA even though it is owned by GDOSYS.
-- ------------------------------------------------------------------------------------------------------------------
-- CONTRIBUTORS:
-- Bruce Hall, Pierre Le Roux, Wayne McGreevy, David McCourt, Martin Hennig, Paul Joyce, 
-- Matthias Uhlenbruck, Jordan Hodges, Lars Eggan, Ron Frebis, Jack Leonard, and many others.
-- A special thanks goes out to all of you who have helped make this package what it is.  I also
-- appreciate all the folks that continue to send me bug reports, suggestions, and even requests.
-- ------------------------------------------------------------------------------------------------------------------
-- RELEASE HISTORY:
-- 01/04/2003  Initial Creation.
-- 01/11/2006  Released.
-- 04/13/2006  Released.
-- 04/18/2007  Released.
-- 03/31/2008  Released.
-- 04/30/2009  Released.
-- 12/03/2009  Released.
-- 04/30/2010  Released.
-- 06/20/2011  Released.
-- 03/15/2012  Released.
-- 07/15/2013  Released.
-- 07/15/2014  Released.
-- 07/15/2015  Released.
-- 07/15/2016  Released.
--
-- HISTORY SINCE LAST RELEASE:
-- 09/10/2015  Altered RTree logic to only use GetGeomType if the option to optimize the index for Geometry Type is True. Also added option to skip statistics.
-- 12/01/2015  Bug Fixes.
-- 07/01/2016  Prep for July release..
-- ------------------------------------------------------------------------------------------------------------------
PROMPT ******************************************************************;
PROMPT **                    GOOM PACKAGE 2016                         **;
PROMPT **     GeoMedia Oracle Object Model Package Installation        **;
PROMPT **                                                              **;
PROMPT ** This package provides procedures and functions that aid in   **;
PROMPT ** the tuning and maintainence of data in Oracle spatial data   **;
PROMPT ** warehouses used by Hexagon''s geospatial applications.       **;
PROMPT **..............................................................**;
PROMPT **                  INSTALLATION INFORMATION                    **;
PROMPT ** -> IMPORTANT: Oracle 11G or later is REQUIRED                **;
PROMPT ** -> Installation requires a DBA connection.  The GDOSYS       **;
PROMPT **    schema is the default installation location.              **;
PROMPT ** -> Oracle 12C has tightened security.  You will need to      **;
PROMPT **    GRANT INHERIT PRIVILEGES ON USER <user> TO GDOSYS;        **;
PROMPT **    where <user> is your DBA user account (like SYSTEM).      **;
PROMPT ** -> The default Tablespace for spatial index storage is USERS **;
PROMPT **    or if it exists, INDX. You can set the default index      **;
PROMPT **    tablespace to your own preference by running:             **;
PROMPT **    EXEC GOOM.SetGOOMIndexSpace('<tablespace_name>')          **;
PROMPT **    <tablespace_name> must be upper case in single quotes.    **;
PROMPT **..............................................................**;
-- ------------------------------------------------------------------------------------------------------------------
-- GOOM Package Definitions
-- ........................................................................

CREATE OR REPLACE PACKAGE GDOSYS.GOOM AUTHID CURRENT_USER IS

-- --------------------------------------------------------------------------------------------------------------------
-- MISC FUNCTIONS: Useful utility functions.
-- ------------------------------------------------------------------------------------------------------------------

-- Returns a random number that is >= i_lo and <= i_hi
  FUNCTION RandInRange( i_lo IN INTEGER, i_hi IN INTEGER) 
           RETURN NUMBER;
-- Returns the base value of the input integer.
  FUNCTION Integer2Base( i_int IN NUMBER, n_base IN NUMBER ) 
           RETURN VARCHAR2;
-- Returns the integer indicated by the base value.
  FUNCTION Base2Integer( v_str IN VARCHAR2,  n_base IN NUMBER DEFAULT 2 ) 
           RETURN NUMBER;
-- Returns the hex equivalent of the input decimal value.
  FUNCTION DEC2HEX ( i_integer IN INTEGER) 
           RETURN VARCHAR2 DETERMINISTIC;
-- Returns the binary equivalent of the input integer.
  FUNCTION Num2Binary ( v_number in NUMBER ) 
           RETURN VARCHAR2;
-- Returns the integer equivalent of the binary input.
  FUNCTION Binary2Num ( v_binary in VARCHAR2 ) 
           RETURN NUMBER;
-- Pads binarys with 0's to round out byte count.
  FUNCTION PadBinary( v_binary IN VARCHAR2 ) 
           RETURN VARCHAR2;
-- Returns the right bit shifted input value.
  FUNCTION RBitShift( i_val IN INTEGER, i_Shift IN INTEGER) 
           RETURN INTEGER;
-- Returns the left bit shifted input value.
  FUNCTION LBitShift( i_val IN INTEGER, i_Shift IN INTEGER) 
           RETURN INTEGER;
-- Returns string describing GM formatted text as stored in Oracle.
  FUNCTION GetGMTextFormat ( v_value in INTEGER) 
           RETURN VARCHAR2;
-- Returns the integer representation of a 4 byte string. 
  FUNCTION Text2Integer( v_string IN VARCHAR2) 
           RETURN INTEGER DETERMINISTIC;
-- Returns a 4 byte string from the input integer.
  FUNCTION Integer2Text(  i_integer IN INTEGER) 
           RETURN VARCHAR2 DETERMINISTIC;
-- Returns a comma delimited string of integers from an input test string.
  FUNCTION String2Integers( v_string IN VARCHAR2) 
           RETURN VARCHAR2;
-- Returns a character string from a string of comma delimited integers.
  FUNCTION Integers2String( v_string IN VARCHAR2) 
           RETURN VARCHAR2;
-- Database Timing Functions: i_start_time:=StartTime;
  FUNCTION StartTime 
           RETURN INTEGER;
-- Database Timing Functions: i_ElapsedTime:=TotalTime(i_Start_Time);
  FUNCTION TotalTime ( i_start_time IN INTEGER ) 
           RETURN NUMBER;
-- Returns the bearing between the first point and the second point.
  FUNCTION Bearing( n_xcoord1 IN NUMBER, n_ycoord1 IN NUMBER, n_xcoord2 IN NUMBER, n_ycoord2 IN NUMBER, 
                    v_ctype IN VARCHAR2 DEFAULT 'P', v_rtype in VARCHAR2 DEFAULT 'D') 
           RETURN NUMBER;
-- Returns decimal degrees from degrees:minutes:seconds.
  FUNCTION DMS2DD ( v_dms IN VARCHAR2 ) 
           RETURN NUMBER;
-- Returns degrees, minutes, seconds from decimal degrees using the specified delimiter.
  FUNCTION DD2DMS ( n_dd IN NUMBER, v_delim IN VARCHAR2 DEFAULT ':' )
           RETURN VARCHAR2;
-- Returns angle in radians from input degrees.
  FUNCTION Ang2Rad ( n_degrees IN NUMBER) 
           RETURN NUMBER;
-- Returns angle in degrees from input radians.
  FUNCTION Rad2Ang ( n_radians IN NUMBER) 
           RETURN NUMBER;
-- Returns the rotation index 'I' or 'J' values for the input angle.
  FUNCTION ROTINDEX ( v_type IN VARCHAR2, n_degrees IN NUMBER) 
           RETURN NUMBER;
  -- Returns the rotation angle value for the iput I,J values.
  FUNCTION ROTANGLE ( n_irot IN NUMBER, n_jrot IN NUMBER) 
           RETURN NUMBER;

-- ------------------------------------------------------------------------------------------------------------------
-- GET FUNCTIONS: These return a specific object name or value
-- ------------------------------------------------------------------------------------------------------------------

--
-- Returns the name of the database instance.
  FUNCTION GetDBNAME 
           RETURN VARCHAR2;
--
-- Returns the current version of the Oracle RDBMS software.
  FUNCTION GetDBVersion 
           RETURN VARCHAR2;
-- Returns the table name in the owner.object format required by GeoMedia.
  FUNCTION GetOwnerObject( v_tablename IN VARCHAR2) 
           RETURN VARCHAR2;
--
-- Returns the indicated component of the owner.object string.
  FUNCTION SplitOwnerObject( v_tablename IN VARCHAR2, v_type IN VARCHAR2) 
           RETURN VARCHAR2;
--
-- Returns the spatial dimensions of the geometry column (2 or 3).
  FUNCTION GetDim( v_tablename IN VARCHAR2, v_geomcol in VARCHAR2) 
           RETURN INTEGER;
--
-- Returns the geometry column name for the table, multiple geometry columns will return an error.
  FUNCTION GetGeom( v_tablename IN VARCHAR2) 
           RETURN VARCHAR2;
--
-- Returns the SRID used by the indicated geometry column using a subsample of the entire rowset.
  FUNCTION GetSRID( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) 
           RETURN INTEGER;
--
-- Returns boolean TRUE if the SRID is geographic.
  FUNCTION isGeographic ( i_srid IN INTEGER DEFAULT 0) 
           RETURN BOOLEAN;
--
-- Returns a auto generated spatial index name for the input table that can be used to create a spatial index.
  FUNCTION GetINDXNAME( v_tablename IN VARCHAR2, v_extend IN VARCHAR2 DEFAULT NULL) 
           RETURN VARCHAR2;
--
-- Returns a auto generated sequence name for the given input table and column. 
  FUNCTION GetSequenceName( v_tablename IN VARCHAR2, 
                               v_column IN VARCHAR2 DEFAULT NULL, 
                               b_unique IN BOOLEAN DEFAULT FALSE) 
           RETURN VARCHAR2;
--
-- Returns the geometry type (GTYPE) of the given geometry using a subsample of the entire rowset.
  FUNCTION GetGTYPE( v_tablename IN VARCHAR2, 
                       v_geomcol IN VARCHAR2 DEFAULT NULL) 
           RETURN VARCHAR2;
--
-- Returns the GDO based geometry type for the input feature class using a subsample of the entire rowset.
  FUNCTION GetGeomType( v_tablename IN VARCHAR2, 
                          v_geomcol IN VARCHAR2 DEFAULT NULL) 
           RETURN VARCHAR2;
--
-- Returns the GDO tablename based on the geomedia metadata tabletype.
  FUNCTION GetGDOTableName ( v_tabletype IN VARCHAR2) 
           RETURN VARCHAR2;
--
-- Returns the row count for the input table.
  FUNCTION GetCount( v_tablename IN VARCHAR2) 
           RETURN INTEGER;
--
-- Returns the coordinate system name for the specified SRID.
  FUNCTION GetCSName ( i_srid IN INTEGER)  
           RETURN VARCHAR2;
--
-- Returns the table segment size in MB.
  FUNCTION GetTableSize( v_tablename IN VARCHAR2) 
           RETURN NUMBER;
--
-- Returns the spatial error explanation for the given validation error.
  FUNCTION GetError( v_error VARCHAR2 DEFAULT 'EMPTY') 
           RETURN VARCHAR2;
--
-- Returns the primary key column for the input table, more than one PK column will return an error.
  FUNCTION GetKeyCol( v_tablename IN VARCHAR2) 
           RETURN VARCHAR2;
--
-- Returns the key column for a view as defined in GDOSYS metadata. 
  FUNCTION GetViewKeyCol( v_viewname IN VARCHAR2) 
           RETURN VARCHAR2;
--
-- Returns the data type of the primary key column.
  FUNCTION GetColumnType ( v_tablename IN VARCHAR2, v_column IN VARCHAR2)
           RETURN VARCHAR2;
--
-- Returns the primary geometry column as defined in GDOSYS metadata.
  FUNCTION GetPrimaryGeom( v_tablename IN VARCHAR2) RETURN VARCHAR2;

-- ------------------------------------------------------------------------------------------------------------------
-- CHECK FUNCTIONS: Returns a BOOLEAN TRUE/FALSE based on condition.
-- ------------------------------------------------------------------------------------------------------------------

-- Returns boolean TRUE if the input object is a view and exists.
  FUNCTION chkView( v_viewname IN VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the input object is a materialized view and exists.
  FUNCTION chkMView( v_viewname IN VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the input object is a table and exists.
  FUNCTION chkTable( v_tablename IN VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the column exists in specified table.
  FUNCTION chkColumn( v_tablename IN VARCHAR2, v_colName in VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the input table/view contains data.
  FUNCTION chkTableData( v_tablename IN VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the input table/view geometry columns exists.
  FUNCTION chkGeometry( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2) RETURN BOOLEAN;
--  
-- Returns boolean TRUE if the input trigger exists.
  FUNCTION chkTrigger( v_trigname IN VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the table is partitioned?
  FUNCTION chkTablePartition( v_tablename IN VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the input sequence name exists.
  FUNCTION chkSequence( v_seqname IN VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the input index name exists.
  FUNCTION chkIndex( v_indexname IN VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the input spatial index name exists.
  FUNCTION chkSpatialIndex( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the table/view has existing GDOSYS metadata.
  FUNCTION chkMetadata( v_tablename IN VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the table/view has existing USER_SDO_GEOM_METADATA metadata.
  FUNCTION chkMBR( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the table has a primary key column.
  FUNCTION chkPKEY( v_tablename IN  VARCHAR2, v_column IN VARCHAR2) RETURN BOOLEAN;
--
-- Returns boolean TRUE if the USER has insert privileges on MDSYS.SDO_GEOM_METADATA_TABLE.
  FUNCTION chkInsertOnMDSYS RETURN Boolean;
--
-- Returns boolean TRUE if the geometry is a GeoMedia text based geometry.
  FUNCTION isText( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2) RETURN BOOLEAN;

-- ------------------------------------------------------------------------------------------------------------------
-- SPATIAL DATA FUNCTIONS AND PROCEDURES: Operates directly on geometry.
-- ------------------------------------------------------------------------------------------------------------------

-- Returns a geometry with the ordinates precision adjusted according to the specified number of decimal places.
  FUNCTION ChangeOrdinatePrecision( g_geometry IN SDO_GEOMETRY, i_decimals IN INTEGER DEFAULT 6) 
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
--
-- Return 0 if geometry contains NO arcs, return 1 if an arc is present.
  FUNCTION ContainsArcs ( g_InputGeom IN SDO_GEOMETRY) 
           RETURN INTEGER;
--
-- Returns a linestring collection geometry from an input polygon geometry.
  PROCEDURE ConvPoly2Line ( v_tablename IN VARCHAR2,
                              v_geomcol IN VARCHAR2 DEFAULT NULL);
--
--Returns a 2D geometry from a 3D input geometry (Z is stripped).
  FUNCTION Convert2D( g_3DGeom IN MDSYS.SDO_GEOMETRY)
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
--
-- Returns the 3D geometry from a 2D input geometry using a constant for Z.
  FUNCTION Convert3D( g_2DGeom IN MDSYS.SDO_GEOMETRY, n_Zval In Number Default 0) 
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
--
-- Returns a native (SDO_POINT_TYPE) geometry from an input oriented point geometry.
  FUNCTION Convert2NativePt( g_geom  IN MDSYS.SDO_GEOMETRY, i_dim IN INTEGER DEFAULT 2) 
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
--
-- Returns an oriented point geometry from an input native point geometry (rotation is 0).
  FUNCTION Convert2OrientedPt( g_geom  IN MDSYS.SDO_GEOMETRY) 
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
--
-- Returns a geometry point cluster based on the vertices of the input polygon. 
  FUNCTION ConvGeom2Pts( g_polygeom in MDSYS.SDO_GEOMETRY) 
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
--
-- Returns the ordinate value of a point geometry specifying X, Y or Z.
  FUNCTION GetPtCoord ( g_geom IN MDSYS.SDO_GEOMETRY, v_typ IN VARCHAR2) 
           RETURN NUMBER;
--
-- Returns the start or end point X,Y or Z value of a line string.
  FUNCTION GetLinearCoord( g_geom IN MDSYS.SDO_GEOMETRY, v_typ IN VARCHAR2, 
                            v_loc IN VARCHAR2 DEFAULT 'START') 
           RETURN NUMBER;
--
-- Returns the bearing in degrees or radians from two input point geometries.
  FUNCTION GetPtBearing( g_geom1 IN SDO_GEOMETRY, g_geom2 in SDO_GEOMETRY, v_rtype IN VARCHAR2 DEFAULT 'D') 
           RETURN NUMBER;
--
-- Returns the linear bearing of a line string (based on start and end points).
  FUNCTION GetLinearBearing( g_geom IN SDO_GEOMETRY, v_rtype IN VARCHAR DEFAULT 'D') 
           RETURN NUMBER;
--
-- Returns the rotation of an oriented point in radians or degrees.
  FUNCTION GetPointRotation( g_geom IN SDO_GEOMETRY, c_type in VARCHAR2 DEFAULT 'D') 
           RETURN NUMBER;
--
-- Sets the rotation of an Oracle oriented point to the value specified in degrees.
FUNCTION SetPointRotation( g_geom IN SDO_GEOMETRY, n_degrees IN NUMBER DEFAULT 0)
         RETURN SDO_GEOMETRY DETERMINISTIC;

-- ------------------------------------------------------------------------------------------------------------------
-- ORACLE METADATA PROCEDURES: Operate on USER_SDO_GEOM_METADATA
-- ------------------------------------------------------------------------------------------------------------------

--
-- Copy the MBR values used by input table to the specified table's metadata.
  PROCEDURE CopyMBR( v_frmtable IN VARCHAR2,
                     v_frmgeom  IN VARCHAR2,
                     v_totable  IN VARCHAR2, 
                      v_togeom  IN VARCHAR2);
--
-- Delete orphan records from USER_SDO_GEOM_METADATA.
  PROCEDURE DeleteOrphanMBR( v_schema IN VARCHAR2 DEFAULT USER);
--
-- Insert the MBR values for the indicated table/geometry column.
  PROCEDURE InsertMBR( i_dim       IN INTEGER,
                       v_tablename IN VARCHAR2,
                       v_geomcol   IN VARCHAR2 DEFAULT NULL,
                       n_xmin      IN NUMBER DEFAULT -2147483648,
                       n_xmax      IN NUMBER DEFAULT 2147483648,
                       n_ymin      IN NUMBER DEFAULT -2147483648,
                       n_ymax      IN NUMBER DEFAULT 2147483648,
                       n_tol       IN NUMBER DEFAULT 0.00005,
                       i_srid      IN INTEGER DEFAULT NULL);
--
-- Set default geographic extents in USER_SDO_GEOM_METADATA for all geometries in given table.
  PROCEDURE SetMBRGeo( v_tablename IN VARCHAR2 DEFAULT 'ALL', i_dimension IN INTEGER DEFAULT NULL);
--
-- Set default projected extents in USER_SDO_GEOM_METADATA for all geometries in given table.
  PROCEDURE SetMBRProj ( v_tablename IN VARCHAR2 DEFAULT 'ALL', i_dimension IN INTEGER DEFAULT NULL);
--
-- Automatically set the USER_SDO_GEOM_METADATA for the specified table/geometry.
  PROCEDURE SetMBR( v_tablename IN VARCHAR2,
                    v_geomcol   IN VARCHAR2 DEFAULT NULL,
                    i_dimension IN INTEGER DEFAULT NULL);
--
-- Automatically set the USER_SDO_GEOM_METADATA for the all tables/geometries in schema.
  PROCEDURE SetMBRAll( v_schema IN VARCHAR2 DEFAULT USER, 
                    i_dimension IN INTEGER DEFAULT NULL);
--
-- Set the SRID of the specific table/geometry.
  PROCEDURE SetSRID( v_tablename IN VARCHAR2,
                     v_geomcol   IN VARCHAR2 DEFAULT NULL,
                     i_srid      IN INTEGER DEFAULT 0);
--
-- Set the SRID of all tables/geometries in schema.
  PROCEDURE SetSRIDAll( i_srid IN INTEGER DEFAULT 0, v_schema in VARCHAR2 DEFAULT USER);
--
-- Set the spatial tolerance for a specified table/geometry.
  PROCEDURE SetSpatialTolerance( v_tablename IN VARCHAR2, 
                                 v_geomcol   IN VARCHAR2 DEFAULT NULL, 
                                 n_tol       IN NUMBER   DEFAULT 0.00005);
--
-- Set the spatial tolerance for all the tables/geometries in the schema.
  PROCEDURE SetSpatialToleranceAll( n_tol   IN NUMBER DEFAULT 0.00005, 
                                   v_schema IN VARCHAR2 DEFAULT USER);
--
-- Returns the spatial tolerance set for a specific table/geometry.
  FUNCTION GetSpatialTolerance( v_tablename IN VARCHAR2, 
                                v_geomcol   IN VARCHAR2 DEFAULT NULL, 
                                v_dim       IN VARCHAR2 DEFAULT 'X') 
           RETURN NUMBER;

-- ------------------------------------------------------------------------------------------------------------------
-- GEOMETRY VALIDATION AND REPAIR PROCEDURES: Geometry operations
-- ------------------------------------------------------------------------------------------------------------------

-- Validate an input table and geometry (Oracle based geometry validation).
  PROCEDURE ValidateGeom( v_tablename IN VARCHAR2, 
                            v_geomcol IN VARCHAR2 DEFAULT NULL);
--
-- Fix redundant points in a feature class geometry.
  PROCEDURE FixRedundantPoints( v_tablename IN VARCHAR2,
                                  v_geomcol IN VARCHAR2 DEFAULT NULL,
                                      n_tol IN NUMBER DEFAULT 0.00005);
--
-- Use Oracle's Rectify procedure to fix a geometry.
  PROCEDURE RepairGeometry( v_tablename IN VARCHAR2, 
                              v_geomcol IN VARCHAR2 DEFAULT NULL, 
                                  n_tol IN NUMBER DEFAULT 0.00005);
--
-- Stroke invalid small arcs in a geometry to line strings.
  PROCEDURE FixSmallArcs ( v_tablename IN VARCHAR2, 
                             v_geomcol IN VARCHAR2 DEFAULT NULL, 
                              v_length IN NUMBER DEFAULT 0.05, 
                                 n_tol IN NUMBER DEFAULT 0.001);
--
-- Fix uninitialized (NULL) geometries by initializing them (EMPTY).
  PROCEDURE FixNullGeoms ( v_schema IN VARCHAR2 DEFAULT USER);
--
-- Remove trailing spaces from all varchar2 fields in the schema.
  PROCEDURE FixTrailingSpaces ( v_schema IN VARCHAR2 DEFAULT USER);
--
--Analyze a single geometry instance.
  FUNCTION AnalyzeGeometry ( g_geometry IN SDO_GEOMETRY, 
                              v_verbose IN VARCHAR2 DEFAULT 'TRUE') 
           RETURN VARCHAR2;

-- ------------------------------------------------------------------------------------------------------------------
-- SPATIAL TUNING PROCEDURES: Spatial indexing
-- ------------------------------------------------------------------------------------------------------------------

-- Autotune a schema after using GeoMedia's Export to Oracle Object Model.
  PROCEDURE AutoTune( v_schema IN VARCHAR2 DEFAULT USER);
--
-- Delete all spatial indexes in schema.
  PROCEDURE DelSidx( v_schema IN VARCHAR2 DEFAULT USER);
--
-- Generate statistics for all objects in schema.
  PROCEDURE STATS( v_schema IN VARCHAR2 DEFAULT USER);
--
-- Delete the statistics for all objects in schema.
  PROCEDURE DELStats( v_schema IN VARCHAR2 DEFAULT USER);
--
-- Create an RTree spatial index (internal only).
  PROCEDURE RTree( v_tablename IN VARCHAR2, 
                   v_geomcol   IN VARCHAR2 DEFAULT NULL, 
                   b_geomopt   IN BOOLEAN DEFAULT TRUE,
                   b_stats     IN BOOLEAN DEFAULT TRUE);
--
-- Automatically create a spatial index for the input table using the options.
  PROCEDURE SpatialIndex( v_tablename IN VARCHAR2, 
                          b_geomopt   IN BOOLEAN DEFAULT TRUE,
                          b_stats     IN BOOLEAN DEFAULT TRUE);
--
-- Automatically create spatial indexes for all feature classes in a schema.
  PROCEDURE SpatialIndexAll( v_schema   IN VARCHAR2 DEFAULT USER, 
                              b_geomopt IN BOOLEAN DEFAULT TRUE,
                              b_stats     IN BOOLEAN DEFAULT TRUE);
--
-- Drop the spatial index associated with a specific table.geometry.
  PROCEDURE DropSidx( v_tablename IN VARCHAR2,
                      v_geomcol   IN VARCHAR2 DEFAULT NULL);

-- ------------------------------------------------------------------------------------------------------------------
-- GEOMEDIA's GDOSYS METADATA PROCEDURES: Working with GDOSYS
-- ------------------------------------------------------------------------------------------------------------------

-- Delete orphan coordinate system records from GDOSYS. 
  PROCEDURE DeleteOrphanCS( b_respn IN BOOLEAN DEFAULT TRUE);
--
-- Set the generic GDOSYS metadata for a specified table or view.
  PROCEDURE SetGDOSYSMetadata( v_tablename   IN VARCHAR2,
                               v_seq_name    IN VARCHAR2 DEFAULT NULL,
                               v_cs          IN VARCHAR2 DEFAULT 'DEFAULT',
                               i_geomtype_in IN INTEGER  DEFAULT NULL,
                               v_keycolumn   IN VARCHAR2 DEFAULT NULL,
                               v_num38       IN VARCHAR2 DEFAULT 'LONG',
                               v_num10       IN VARCHAR2 DEFAULT 'DOUBLE');
--
-- Set generic GDOSYS metadata for all objects in the schema.
  PROCEDURE SetGDOSYSMetadataAll( v_schema IN VARCHAR2 DEFAULT USER);
--        
-- Set the default coordinate system for the schema.
  PROCEDURE SetDefaultCS( v_csguid in VARCHAR2 DEFAULT NULL, v_schema IN VARCHAR2 DEFAULT USER);
--
-- Set the primary geometry metadata for the indicated table/geometry.
  PROCEDURE SetPrimaryGeom( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2);
--
-- Set GDOSYS metadata override for numeric and integer based columns.
  PROCEDURE OverrideNumDatatype( v_tablename IN VARCHAR2, v_column IN VARCHAR2, v_type IN VARCHAR2);
--    
-- Set GDOSYS metadata hypertext flag for a character based field.    
  PROCEDURE SetField2Hypertext( v_tablename IN VARCHAR2, v_column IN VARCHAR2);       
-- Delete the GDOSYS metadata for the specified table.                 
  PROCEDURE DelGDOSYSMetadata( v_tablename IN VARCHAR2, b_respn IN BOOLEAN DEFAULT TRUE);
-- DBA Only. Delete the GDOSYS metadata for all non-existent users..
  PROCEDURE DBADelGDOSYSOrphans;
--
-- Delete orphaned GDOSYS metadata.
  PROCEDURE DelGDOSYSOrphans( v_schema IN VARCHAR2 DEFAULT USER);
--
-- Log a modification record in GDOSYS.MODIFICATIONLOG (best used with a trigger or procedure).
  PROCEDURE LogDataModification( v_tablename IN VARCHAR2, 
                                   v_keyname IN VARCHAR2, 
                                  v_keyvalue IN VARCHAR2, 
                                   i_modtype IN INTEGER);
--
-- Truncate the GDOSYS MODIFIEDTABLES and MODIFICATIONLOG tables (use with jobs).
  PROCEDURE ClearModLog ( v_user IN VARCHAR2 DEFAULT NULL);

-- ------------------------------------------------------------------------------------------------------------------
-- UTILITY PROCEDURES: Internal messaging, formatting, and utilities.
-- ------------------------------------------------------------------------------------------------------------------

-- Returns the package version.
  PROCEDURE VERSION;
-- Writes message to SERVEROUTPUT.
  PROCEDURE DBMSG( v_Msg    IN VARCHAR2,
                   i_maxlen IN INTEGER DEFAULT 255);
-- Writes feedback from running packages to SERVEROUTPUT.
  PROCEDURE Response( v_operation IN VARCHAR2, 
                      v_results   IN VARCHAR2, 
                      i_pad       IN INTEGER DEFAULT 30, 
                      b_RtJustify IN BOOLEAN DEFAULT FALSE);
--
-- Writes a dashed line of specified length to SERVEROUTPUT.
  PROCEDURE DashLine( i_length IN INTEGER DEFAULT 80);
-- Writes a dotted line of specified length to SERVEROUTPUT.
  PROCEDURE DotLine( i_length IN INTEGER DEFAULT 80);
-- Writes a double line of specified length to SERVEROUTPUT.
  PROCEDURE DblLine( i_length IN INTEGER DEFAULT 80);
-- Writes a line of any specified character to specified length to SERVEROUTPUT.
  PROCEDURE GenLine( v_char IN VARCHAR2, i_length IN INTEGER DEFAULT 80);
-- Writes a process start message for the indicated procedure to SERVEROUTPUT.
  PROCEDURE ProcessStart    ( v_cmdname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER);
-- Writes a process complete message for the indicated procedure to SERVEROUTPUT.
  PROCEDURE ProcessComplete ( v_cmdname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER);
-- Writes a process terminaed due to error message, used in exception block.
  PROCEDURE ProcessTerminate ( v_cmdname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER);
-- Writes a formatted titleblock based on the input information to SERVEROUTPUT.
  PROCEDURE TitleBlock( v_title IN VARCHAR2, 
                     i_length   IN INTEGER  DEFAULT 80, 
                        v_sep   IN VARCHAR2 DEFAULT '-');
-- Writes a formatted title line based on the input information to SERVEROUTPUT.
  PROCEDURE TitleLine( v_title IN VARCHAR2, 
                    i_length   IN INTEGER  DEFAULT 80, 
                       v_sep   IN VARCHAR2 DEFAULT '-');
--
-- Writes function/procedure results to GOOM_log.
  PROCEDURE WRITE_RESULTS( v_schema   IN VARCHAR2 DEFAULT USER,
                            v_type    IN VARCHAR2, 
                            v_cmd     IN VARCHAR2, 
                            v_feature IN VARCHAR2, 
                            v_result  IN VARCHAR2);
--
-- Writes a standard formatted error message to SERVEROUTPUT.
  PROCEDURE REPORT_ERROR( v_cmdname IN VARCHAR2, 
                          v_process IN VARCHAR2,
                            v_debug IN VARCHAR2, 
                          v_SQLCODE IN VARCHAR2, 
                          v_SQLERRM IN VARCHAR2);
--
-- Returns the tablespace that will be used for Spatial indexing.
  FUNCTION GetGOOMIndexSpace RETURN VARCHAR2;
--
-- Sets the tablespace that is to be used for spatial indexes.
  PROCEDURE SetGOOMIndexSpace( v_indexspace IN VARCHAR2);

-- ------------------------------------------------------------------------------------------------------------------
-- TABLE AND SEQUENCE PROCEDURES: Database object creation and manipulation
-- ------------------------------------------------------------------------------------------------------------------

-- Drop specified table and all associated metadata.
  PROCEDURE DropTable( v_tablename IN VARCHAR2);
--
-- Make a copy of an existing table, recreate the spatial indexes and the metadata.
  PROCEDURE CopyTable( v_srcownertable IN VARCHAR2, 
                       v_tgtownertable IN VARCHAR2,
                       b_sidx          IN BOOLEAN DEFAULT TRUE,
                       b_data          IN BOOLEAN DEFAULT TRUE,
                       b_gdosys        IN BOOLEAN DEFAULT TRUE);
--
-- Delete row duplicates based on specified column.
  PROCEDURE DelDupRows( v_tablename IN VARCHAR2, v_column IN VARCHAR2);
--
-- Create sequence for the input table and key column.
  PROCEDURE CreateSequence( v_tablename IN VARCHAR2, 
                            v_Column    IN VARCHAR2 DEFAULT NULL);
--
-- Create a new sequence and return the auto generated sequence name.
  PROCEDURE CreateNewSequence( v_tablename IN VARCHAR2, 
                               v_Column    IN VARCHAR2 DEFAULT NULL, 
                               v_seqname   OUT VARCHAR2);
--
-- Drop the specified sequence.
  PROCEDURE DropSequence( v_sequence IN VARCHAR2);
--
-- Add a new primary key column.
  PROCEDURE AddPrimaryKey( v_table IN VARCHAR2, 
                           v_keycolnew IN VARCHAR2 DEFAULT 'PID', 
                           b_dropflag in BOOLEAN DEFAULT FALSE);
-- ------------------------------------------------------------------------------------------------------------------
-- ONLINE HELP
-- ------------------------------------------------------------------------------------------------------------------
  
-- Writes online help to console SERVEROUTPUT.
  PROCEDURE HELPME;
-- Online help syntax information.
  PROCEDURE HelpSyntax;
-- Data manipulation procedures
  PROCEDURE HelpDATA;
-- Oracle metadata procedures.
  PROCEDURE HelpMBR;
-- Geomedia metadata (GDOSYS) procedures.
  PROCEDURE HelpGDOSYS;
-- Table related procedures.
  PROCEDURE HelpTables;
-- Spatial Indexing and tuning.
  PROCEDURE HelpTUNING;
  -- Miscellaneous GOOM utilities.
  PROCEDURE HelpUtilities;
-- Data Validation and repair procedures.
  PROCEDURE HelpVALIDATION;
-- GOOM utility functions.
  PROCEDURE HelpFUNCTIONS; 

-- ------------------------------------------------------------------------------------------------------------------
END;
/
show errors
  --------------------------------------------------------------------------------------------------------------------
  -- The GOOM Package Body (Procedures and Functions)
  --------------------------------------------------------------------------------------------------------------------
CREATE OR REPLACE PACKAGE BODY GDOSYS.GOOM IS
  --------------------------------------------------------------------------------------------------------------------
  -- DBA definable defaults, set these parameters to suit your needs prior to installing package:
  -- -----------------------------------------------------------------------------------------------------------------

  c_indxspace   CONSTANT VARCHAR2(30) := 'USERS';   -- Change this variable to tablespace you want spatial indexing to
  --                                                   use if none is specified.  Process will automatically use INDX
  --                                                   if available.  Use SetGOOMIndexSpace to permanently set the
  --                                                   index tablespace to use for spatial indexes.
  c_tol         CONSTANT NUMBER       := 0.00005;   -- Sets the tolerance used by Oracle in Validation Calculations
  --                                                   If you are using Geomedia, c_tol should never be larger than 
  --                                                   0.0001 m or smaller than 1/2 GM: 0.00005
  c_gtol        CONSTANT NUMBER       := 0.05;      -- Sets the tolerance used by Oracle in Geodetic Calculations
  --                                                   Oracle looks at this value in meters, 0.05 is smallest allowed.
  c_defDim      CONSTANT PLS_INTEGER  := 3;         -- Default feature class dimension:  2 for 2D, 3 for 3D.
  --                                                   This dimension is applied to empty geometry columns whose actual  
  --                                                   dimension is unspecified.  If you use 2, remember that the Z entry
  --                                                   in GDOSYS.GPARAMETERS has to be deleted. By default GM is 3D.
  c_logging     CONSTANT VARCHAR2(3)  :='NO';       -- Package Command Logging: YES turns on process logging to the table
  --                                                   OWNER.GOOM_LOG (created automatically).  Logging is NO by default.
  c_valext      CONSTANT VARCHAR2(4)  := '_ERR';    -- Geometry validation table name extension. Max 4 chars!
  --
  c_idxext      CONSTANT VARCHAR2(4)  := '_SI';     -- Spatial index name extension (table_SI). Max 4 chars!
  --
  c_PKext       CONSTANT VARCHAR2(4)  := '_PK';     -- Primary Key constraint name extension. Max 4 chars!
  --
  c_sample      CONSTANT PLS_INTEGER  := 1000;      -- Smaller value increases performance but may limit accurate results.
  --                                                -- This value is used in some GET functions when sampling geometries 
  --                                                   for distinct properties such as GTYPE. 
  -- 
  -- Default Extents for SetMBR and SetMBRProj (USER_SDO_GEOM_METADATA).  
  -- Set these if you only work in a specific area and you want to utilize Zoom to Extents capability with 
  -- your data.  GM apps will not use this so you generally do not need to change these.  Keep in mind that 
  -- these changes are global and will affect all the schemas in your database.
  -- The default for LO is -2147483648, and HI is 2147483648.  This does not change InsertMBR defaults, 
  -- nor do they change the defaults for geographic geometries (+-180, +-90). 
  --
  c_XLO         CONSTANT NUMBER       := -2147483648;
  c_YLO         CONSTANT NUMBER       := -2147483648;
  c_ZLO         CONSTANT NUMBER       := -2147483648;
  c_XHI         CONSTANT NUMBER       :=  2147483648;
  c_YHI         CONSTANT NUMBER       :=  2147483648;
  c_ZHI         CONSTANT NUMBER       :=  2147483648;
  -- -----------------------------------------------------------------------------------------------------------------
  -- -----------------------------------------------------------------------------------------------------------------
  -- DO NOT CHANGE THESE PACKAGE CONSTANTS : This is the versioning for the main package
  c_pkg_version CONSTANT VARCHAR2(10) := '2016.007';
  c_pkg_date    CONSTANT VARCHAR2(10) := '08/01/2016';
  -- CHANGE THESE INSTEAD: Use these if you make modifications to the original package and you want to track those 
  -- changes.  
  c_mod_author  CONSTANT VARCHAR2(10) := 'YourName'; 
  c_mod_version CONSTANT VARCHAR2(10) := '0.00'; 
  c_mod_date    CONSTANT VARCHAR2(10) := '00/00/0000';
  -- -----------------------------------------------------------------------------------------------------------------
  -- PI is used in some internal calculations with arcs and point rotation.
  pi            CONSTANT NUMBER := 3.1415926535897932384626433832795;
  -- -----------------------------------------------------------------------------------------------------------------
  -- Global Cursor Declarations 
  -- -----------------------------------------------------------------------------------------------------------------

  -- Return list of tables containing geometry column in the specified schema.
  CURSOR GetGeomBasedFCs ( v_schema VARCHAR2 DEFAULT USER ) IS
    SELECT TABLE_NAME, COLUMN_NAME
      FROM ALL_TAB_COLUMNS
     WHERE DATA_TYPE = 'SDO_GEOMETRY'
       AND TABLE_NAME NOT LIKE 'BIN$%'
       AND TABLE_NAME NOT LIKE 'RDT_%$'
       AND OWNER = v_schema;
  --
  -- Return list of spatial indexes in the specified schema.
  CURSOR GetSpatialIndexNames ( v_schema VARCHAR2 DEFAULT USER) IS
    SELECT INDEX_NAME
      FROM ALL_INDEXES
     WHERE OWNER = v_schema
       AND INDEX_TYPE = 'DOMAIN'
       AND TABLE_NAME NOT LIKE 'BIN$%';
  --
  -- Return a list of tables and geometries from Oracle's ALL_SDO_GEOM_METADATA table.
  CURSOR GetTableGeomsFromAllMBR ( v_schema VARCHAR2 DEFAULT USER) IS
    SELECT TABLE_NAME, COLUMN_NAME 
      FROM ALL_SDO_GEOM_METADATA
     WHERE OWNER = v_schema;
  --
  -- Return a list of tables and geometries from Oracle's USER_SDO_GEOM_METADATA table.
  CURSOR GetTableGeomsFromUserMBR IS
    SELECT TABLE_NAME, COLUMN_NAME 
      FROM USER_SDO_GEOM_METADATA;
  --
  -- Return a list of tables in the specified schema, excluding recycle bin and MDRT/XT.
  CURSOR GetTableNames ( v_schema VARCHAR2 DEFAULT USER) IS
    SELECT TABLE_NAME 
      FROM ALL_TABLES
     WHERE OWNER = v_schema
       AND TABLE_NAME NOT LIKE 'MDRT_%$' 
       AND TABLE_NAME NOT LIKE 'MDXT_%' 
       AND TABLE_NAME NOT LIKE 'BIN$%';
  --
  -- -----------------------------------------------------------------------------------------------------------------
  -- Global ERROR and EXCEPTION Handling --  messages can be changed to accommodate languages 
  -- -----------------------------------------------------------------------------------------------------------------
  --
  e_CannotBeNull        EXCEPTION;  -- Use when checking if NULLs are allowed.
  e_ColumnNotFound      EXCEPTION;  -- Use when checking for column existence.
  e_DifferentSRID       EXCEPTION;  -- Use when checking if SRIDs match.
  e_GeometryNotFound    EXCEPTION;  -- Use when checking geometry existence.
  e_InvalidDimension    EXCEPTION;  -- Use when checking specified geometry dimension.
  e_InvalidGType        EXCEPTION;  -- Use when checking GTYPE.
  e_InvalidInput        EXCEPTION;  -- Use when checking input parameters.
  e_InvalidDataType     EXCEPTION;  -- Use when checking the datatype.
  e_InvalidPTType       EXCEPTION;  -- Use when checking whether point geometry is valid.
  e_InvalidRowCount     EXCEPTION;  -- Use when checking if the row count is valid.
  e_InvalidSRID         EXCEPTION;  -- Use when checking if SRID is valid.
  e_NoDataFound         EXCEPTION;  -- Use when checking data existence.
  e_NoMetadataAccess    EXCEPTION;  -- Use when checking Oracle metadata access privilege.
  e_NoMetadataFound     EXCEPTION;  -- Use when checking Oracle metadata existence.
  e_NoGDOSYSMetadata    EXCEPTION;  -- Use when checking GDOSYS metadata existence.
  e_NoPrivilege         EXCEPTION;  -- Use when checking privilege for operation.
  e_NullAzimuth         EXCEPTION;  -- Use when checking if Azimuth is NULL.
  e_ObjNotFound         EXCEPTION;  -- Use when checking if a generic database object is not found.
  e_SequenceNotFound    EXCEPTION;  -- Use when checking sequence existence.
  e_TableIsView         EXCEPTION;  -- Use when checking if table is really a view.
  e_TableNotFound       EXCEPTION;  -- Use when checking for table existence.
  e_TabOwnerError       EXCEPTION;  -- Owner Privilege error.
  e_TabSpaceNotFound    EXCEPTION;  -- Use when checking tablespace existence.
  --
  -- PRAGMA exceptions : Take over the error processing from the system.
  --
  PRAGMA EXCEPTION_INIT(e_TableNotFound, -942); -- Pragma for table not found.
  PRAGMA EXCEPTION_INIT(e_NoDataFound,    100); -- Pragma for data not found.
  PRAGMA EXCEPTION_INIT(e_NoPrivilege,  -1031); -- Pragma for privilege not found.
  --
  -- General Exception Error Messages
  --
  c_msgCannotBeNull     CONSTANT VARCHAR2(70) := 'Input Parameters Cannot be Null: ';
  c_msgColumnNotFound   CONSTANT VARCHAR2(70) := 'The specified column does not exist: ';
  c_msgComplete         CONSTANT VARCHAR2(30) := 'Process Complete for: ';  
  c_msgDifferentSRID    CONSTANT VARCHAR2(70) := 'The SRID''s do not match.';
  c_msgGeometryNotFound CONSTANT VARCHAR2(70) := 'Specified geometry column could not be found: '; 
  c_msgGoomInternal     CONSTANT VARCHAR2(38) := 'GOOM - An Internal Error has occurred.';
  c_msgInvalidInput     CONSTANT VARCHAR2(70) := 'The input values are invalid: ';
  c_msgInvalidDataType  CONSTANT VARCHAR2(70) := 'The data type is invalid for: ';
  c_msgInvalidDimension CONSTANT VARCHAR2(70) := 'The geometry dimensions are not valid for this operation.';
  c_msgInvalidGType     CONSTANT VARCHAR2(70) := 'Invalid GTYPE in input geometry: ';
  c_msgInvalidRowCount  CONSTANT VARCHAR2(70) := 'Invalid Row Count for Object.';
  c_msgInvalidSRID      CONSTANT VARCHAR2(70) := 'The SRID used is invalid.';
  c_msgInvalidPTType    CONSTANT VARCHAR2(30) := 'Invalid Point type';
  c_msgNoDataFound      CONSTANT VARCHAR2(70) := 'Specified data could not be found: ';
  c_msgNoMetadataAccess CONSTANT VARCHAR2(70) := 'No insert/delete privileges on MDSYS.SDO_GEOM_METADATA_TABLE for: ';
  c_msgNoMetadataFound  CONSTANT VARCHAR2(70) := 'No metadata found in ALL_GEOM_METADATA_TABLE for: ';
  c_msgNoGDOSYSMetadata CONSTANT VARCHAR2(70) := 'No GDOSYS metadata found for: ';
  c_msgNoPrivilege      CONSTANT VARCHAR2(70) := 'User does not have privilege for this operation.';
  c_msgNoSIDX           CONSTANT VARCHAR2(42) := 'The table does NOT have a spatial index: ';
  c_msgNoKeyColumn      CONSTANT VARCHAR2(25) := 'No key column assigned.';
  c_msgNone             CONSTANT VARCHAR2(10) := 'No Message';
  c_msgNullAzimuth      CONSTANT VARCHAR2(70) := 'Azimuth Calculation returns NULL.';
  c_msgOraError         CONSTANT VARCHAR2(25) := 'Oracle Error: ORA';
  c_msgSequenceNotFound CONSTANT VARCHAR2(70) := 'Specified sequence could not be found: ';
  c_msgSRIDIsNull       CONSTANT VARCHAR2(46) := 'NULL SRID indicated - OK for projected data: ';
  c_msgStart            CONSTANT VARCHAR2(30) := 'Process Started for: ';
  c_msgTableNotFound    CONSTANT VARCHAR2(70) := 'The specified table or view could not be found: ';
  c_msgTableExists      CONSTANT VARCHAR2(70) := 'The specified table or view already exists: ';
  c_msgTableIsView      CONSTANT VARCHAR2(70) := 'A view has been specified - no processing required: ';
  c_msgTabOwnerError    CONSTANT VARCHAR2(70) := 'Only the owner of the table is allowed to do this: ';
  c_msgTabSpaceNotFound CONSTANT VARCHAR2(70) := 'Specified tablespace does not exist: ';
  c_msgTerminate        CONSTANT VARCHAR2(30) := 'Process Terminated for: ';
  --
  -- Procedure Exception Responses
  --
  c_msgError            CONSTANT VARCHAR2(10) := ' - ERROR';
  c_msgInform           CONSTANT VARCHAR2(10) := ' - INFORM';
  c_msgSolution         CONSTANT VARCHAR2(11) := ' - SOLUTION';
  c_msgVerify           CONSTANT VARCHAR2(10) := ' - VERIFY';
  c_msgWarning          CONSTANT VARCHAR2(10) := ' - WARNING';
  --
  -- -----------------------------------------------------------------------------------------------------------------
  -- UTILITY PROCEDURES: Internal messaging, formatting, and utilities.
  -- -----------------------------------------------------------------------------------------------------------------
  --
  -- ------------------------------------------------------------------------------
  -- EXEC GOOM.version;
  -- Version and date information. PLEASE DO NOT MODIFY!
  PROCEDURE VERSION IS
  c_support     CONSTANT VARCHAR2(14) := 'Via Email Only';
  c_emailpath   CONSTANT VARCHAR2(29) := 'chuck.woodbury@hexagonsi.com';
  c_onlinehelp  CONSTANT VARCHAR2(16) := 'EXEC GOOM.HELPME';
  i_width       PLS_INTEGER           :=66;
  BEGIN
    DblLine( i_width );
    TitleLine('Hexagon Safety and Infrastructure/Hexagon Geospatial', i_width,'**');
    TitleLine('GeoMedia Oracle Object Model Package', i_width,'**');
    TitleLine('GOOM PKG', i_width,'**');
    DblLine( i_width );
    TitleLine('Author: Chuck Woodbury, Senior Technical Consultant', i_width,'**');
    TitleLine('Hexagon Technology Services', i_width,'**');
    DblLine( i_width );
    Response('Version', c_pkg_version, i_width, TRUE);
    Response('Date', c_pkg_date, i_width, TRUE);
    Response('Spatial Index Tablespace', GetGOOMIndexSpace , i_width, TRUE);
    Response('Bug Reports and Support', c_support, i_width, TRUE);
    Response('Email', c_emailpath, i_width, TRUE);
    Response('Online Help', c_onlinehelp, i_width, TRUE);
    DblLine( i_width );
    IF c_mod_version <> '0.00' THEN
        Response('Custom Author', c_mod_version, i_width, TRUE);
        Response('Custom Version', c_mod_version, i_width, TRUE);
        Response('Custom Date', c_mod_date, i_width, TRUE);
        DblLine( i_width );
    END IF;
  END VERSION;
  -- ------------------------------------------------------------------------------
  -- DBMsg is used to send DBMS_OUTPUT messages back to the console.  Messages require 
  -- SERVEROUTPUT to be on -> SET SERVEROUTPUT ON
  -- Syntax: EXEC GOOM.DBMSG(c_text, [i_max_length]);
  --         Input c_text, default max length is 255 characters.
  --
  PROCEDURE DBMSG( v_Msg IN VARCHAR2, i_maxlen IN INTEGER DEFAULT 255) IS
    v_startPos PLS_INTEGER;  -- Start position of text
    v_len      PLS_INTEGER;  -- Actual length of message.
  BEGIN
    DBMS_OUTPUT.ENABLE(1000000);
    v_len := LENGTH( v_Msg );
    IF ( v_len < i_maxlen ) THEN
      DBMS_OUTPUT.PUT_LINE( v_Msg );
    ELSE
      v_startPos := 1;
      LOOP
        IF ( v_startPos < v_len ) THEN
          DBMS_OUTPUT.PUT_LINE(SUBSTR( v_Msg, v_startPos, i_maxlen ));
          v_startPos := v_startPos + i_maxlen;
        ELSE
          EXIT;
        END IF;
      END LOOP;
    END IF;
  END DBMSG;
  -- ------------------------------------------------------------------------------
  -- Response uses DBMsg to send a formatted response from a procedure or function. 
  -- Syntax: EXEC GOOM.Response(v_operation,v_result,[i_pad],[b_rjustify]');
  --         v_operation : the procedure name or anything else up to 30 characters.
  --         v_results   : the information you want to pass to the user.
  --         i_pad       : number of spaces between operation and result (default 30).
  --         b_RtJustify : determines if the output should be right justified (default FALSE).
  --
  PROCEDURE Response( v_operation IN VARCHAR2, v_results IN VARCHAR2, i_pad IN INTEGER DEFAULT 30, b_RtJustify IN BOOLEAN DEFAULT FALSE) AS
    v_oplength  PLS_INTEGER;
    i_padadj    PLS_INTEGER;
    v_newpad    PLS_INTEGER;
  BEGIN
    v_oplength := LENGTH( v_operation );
    IF v_oplength >= i_pad THEN
      v_newpad := v_oplength + 3;
    ELSE
      v_newpad := i_pad;
    END IF;
    IF NOT b_RtJustify THEN
      DBMSG(RPAD( v_operation, v_newpad,'.')||' '|| v_results);
    ELSE
      i_padadj := v_newpad - LENGTH( v_results) - 1;
      DBMSG(RPAD( v_operation, i_padadj,'.')||' '|| v_results);
    END IF;
  END;
  -- ------------------------------------------------------------------------------
  -- Generate a dashed (--) line of specified length.
  -- Syntax:  EXEC GOOM.DashLine([i_length]);
  --          Default length is 80.
  PROCEDURE DashLine( i_length IN INTEGER DEFAULT 80) AS
  BEGIN
    DBMSG(RPAD('-', i_length,'-'));
  END;
  -- ------------------------------------------------------------------------------
  -- Generate a dotted (...) line of specified length.
  -- Syntax:  EXEC GOOM.DotLine([i_length]);
  --          Default length is 80.
  PROCEDURE DotLine( i_length IN INTEGER DEFAULT 80) AS
  BEGIN
    DBMSG(RPAD('.', i_length,'.'));
  END;
  -- ------------------------------------------------------------------------------
  -- Generate a double (===) line of specified length.
  -- Syntax:  EXEC GOOM.DblLine([i_length]);
  --          Default length is 80.
  PROCEDURE DblLine( i_length IN INTEGER DEFAULT 80) AS
  BEGIN
    DBMSG(RPAD('=', i_length,'='));
  END;
  -- ------------------------------------------------------------------------------
  -- Generate a line of any character with a specified length.
  -- Syntax:  EXEC GOOM.GenLine(v_char, [i_length]);
  --          Default length is 80.
  PROCEDURE GenLine( v_char IN VARCHAR2, i_length IN INTEGER DEFAULT 80) AS
   v_onechar    VARCHAR2(1):=SUBSTR( v_char,1,1);
  BEGIN
    DBMSG(RPAD(v_onechar,i_length,v_onechar));
  END;
  -- ------------------------------------------------------------------------------
  -- Generate a formatted title block.
  -- Syntax:  EXEC GOOM.TitleBlock(v_title,[i_length], [v_v_sep]);
  --          c_title   : the centered title line (required).
  --          i_length  : the length of the title block, default 80.
  --          c_separator : is the character separator, default is -.
  PROCEDURE TitleBlock( v_title IN VARCHAR2, i_length IN INTEGER DEFAULT 80, v_sep IN VARCHAR2 DEFAULT '-') is
    c_cmdname       CONSTANT VARCHAR2(10) := 'TitleBlock';
    v_titleLength   PLS_INTEGER;
    v_sepLength     PLS_INTEGER;
    v_titleEnd      PLS_INTEGER;
    i_padadj        PLS_INTEGER;
  BEGIN
     v_titleLength  := LENGTH( v_title );
  	 v_sepLength    := LENGTH( v_sep );
  	 IF v_titleLength >= i_length - ( v_sepLength * 2) THEN
  	   i_padadj := 4 + v_titleLength + ( v_sepLength * 2);
  	 ELSE
  	   i_padadj := i_length;
  	 END IF;
     v_titleEnd := FLOOR( i_padadj / 2) + FLOOR( v_titleLength / 2) - v_sepLength;
     DashLine( i_padadj);
  	 DBMSG( v_sep||(RPAD(LPAD( v_title, v_titleEnd,' '), i_padadj-(2 * v_sepLength),' '))|| v_sep);
     DotLine( i_padadj);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_title, i_length||'.'|| v_sep, SQLCODE, SQLERRM );
  END TitleBlock;
  -- ------------------------------------------------------------------------------
  -- Generate a title line.
  -- Syntax:  EXEC GOOM.TitleLine(v_title,[i_length], [v_v_sep]);
  --          v_title : the centered title line (required).
  --          i_length : the length of the title block, default 80.
  --          v_separator : the character separator, default is -.
  PROCEDURE TitleLine( v_title IN VARCHAR2, i_length IN INTEGER DEFAULT 80, v_sep IN VARCHAR2 DEFAULT '-') is
    c_cmdname       CONSTANT VARCHAR2(9) := 'TitleLine';
    v_titleLength   PLS_INTEGER;
    v_sepLength     PLS_INTEGER;
    v_titleEnd      PLS_INTEGER;
    i_padadj        PLS_INTEGER;
  BEGIN
     v_titleLength  := LENGTH( v_title );
  	 v_sepLength    := LENGTH( v_sep );
  	 IF v_titleLength >= i_length - ( v_sepLength * 2) THEN
  	   i_padadj := 4 + v_titleLength + ( v_sepLength * 2);
  	 ELSE
  	   i_padadj := i_length;
  	 END IF;
     v_titleEnd := FLOOR( i_padadj / 2) + FLOOR( v_titleLength / 2) - v_sepLength;
  	 DBMSG( v_sep||(RPAD(LPAD( v_title, v_titleEnd,' '), i_padadj-(2 * v_sepLength),' '))|| v_sep);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_title, i_length||'.'|| v_sep, SQLCODE, SQLERRM);
  END TitleLine;
  -- ------------------------------------------------------------------------------
  -- Indicate that a process has started (used when running a string of procedures).
  -- Syntax:  EXEC ProcessStart(v_procname, v_schema);
  --          v_cmdname : the name of the running process.
  --          v_schema  : the user/schema where process is running.
  PROCEDURE ProcessStart( v_cmdname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER) IS
  BEGIN
      DBLLINE;
      RESPONSE( v_cmdname, c_msgStart|| v_schema,25);
      DotLine;
  END ProcessStart;
  -- ------------------------------------------------------------------------------
  -- Indicate that a process has ended (used when running a string of procedures).
  -- Syntax:  EXEC ProcessComplete(v_cmdname, v_schema);
  --          v_cmdname : the name of the running process.
  --          v_schema  : the user/schema where process is running.
  PROCEDURE ProcessComplete ( v_cmdname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER) IS
  BEGIN
      DotLine;
      RESPONSE( v_cmdname, c_msgComplete || v_schema,25);
      DBLLINE;
  END ProcessComplete;
  -- ------------------------------------------------------------------------------
  -- Indicate that a group of processes has terminted due to error.
  -- Syntax:  EXEC ProcessTerminate(v_cmdname, v_schema);
  --          v_cmdname : the name of the running process.
  --          v_schema : the user/schema where process is running.
  PROCEDURE ProcessTerminate ( v_cmdname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER) IS
  BEGIN
      DotLine;
      RESPONSE( v_cmdname, c_msgTerminate || v_schema,25);
      DBLLINE;
  END ProcessTerminate;
  -- ------------------------------------------------------------------------------
  -- Output a formatted error message to the end user.
  -- Syntax:  EXEC GOOM.REPORT_ERROR (v_cmdname,v_process,v_debug,v_SQLCODE,v_SQLERRM);
  --          v_cmdname : the name of the procedure/function generating the error.
  --          v_process : the process being run when error occurred. 
  --          v_debug : a debug msg embedded in the procedure/function.
  --          v_SQLCODE,v_SQLERRM are system level codes from Oracle.
  PROCEDURE REPORT_ERROR( v_cmdname IN VARCHAR2, v_process IN VARCHAR2, v_debug IN VARCHAR2, 
                         v_SQLCODE IN VARCHAR2, v_SQLERRM IN VARCHAR2) IS
  c_cmdname VARCHAR2(12) := 'REPORT_ERROR';
  BEGIN
    titleblock('WARNING - AN ERROR HAS OCCURRED',80,'****');
    DBMSG('Error occurred in: ' || v_cmdname);
    DBMSG('-while processing: ' || v_process);
    DBMSG('Debug Information: ' || v_debug);
    DBMSG('Error Information: ' || v_SQLCODE);
    DBMSG('ORA Error message: ' || v_SQLERRM);
    DBLLINE(80);
  EXCEPTION
    WHEN OTHERS THEN
      RESPONSE( c_cmdname, 'Failure to write error msg!');
  END REPORT_ERROR;
  -- ------------------------------------------------------------------------------
  -- Write operations and results to the GOOM_LOG table for later review if logging is turned on.  
  -- Syntax: EXEC GOOM.WRITE_RESULTS(v_type,v_cmd,v_feature,v_result);
  --         v_type : the category of the process, use to sort log table.
  --         v_cmd : the procedure or function that is running.
  --         v_feature : the feature class being processed.
  --         v_result : the result being logged.
  PROCEDURE WRITE_RESULTS( v_schema IN VARCHAR2 DEFAULT USER, v_type IN VARCHAR2, 
                           v_cmd   IN VARCHAR2, v_feature IN VARCHAR2, v_result IN VARCHAR2) IS
    c_cmdname CONSTANT VARCHAR2(14) := 'WRITE_RESULTS';
    v_sql     VARCHAR2(512);
    v_date    TIMESTAMP;
    v_debug   VARCHAR2(30) := 'INIT';
  BEGIN
    IF c_logging = 'YES' THEN
      v_date := SYSDATE;
      IF NOT chkTable( v_schema || '.GOOM_LOG' ) THEN
        v_sql := 'CREATE TABLE '|| v_schema ||'.GOOM_LOG (OP_DATE TIMESTAMP, OP_TYPE VARCHAR2(64), GOOM_CMD VARCHAR2(64), APPLIES_TO VARCHAR2(255), DESCRIPTION VARCHAR2(512))';
        EXECUTE IMMEDIATE v_sql;
      END IF;
      v_sql := 'INSERT INTO '|| v_schema ||'.GOOM_LOG VALUES(:vdate,:vtype,:vcmd,:vfeature,:vresult)';
      EXECUTE IMMEDIATE v_sql USING v_date, v_type, v_cmd, v_feature, v_result;
      COMMIT;
      v_debug := 'Inserted log values.';
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_sql, v_debug, SQLCODE, SQLERRM);
  END WRITE_RESULTS;
  ---------------------------------------------------------------------------------
  -- Returns the INDX space currently used by the spatial indexing commands.
  -- Example: Select GetGOOMIndexSpace from dual;
  FUNCTION GetGOOMIndexSpace RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(17):='GetGOOMIndexSpace';
    v_indexspace    VARCHAR2(30);
    i_count         PLS_INTEGER;
  BEGIN
    v_indexspace := c_indxspace;   -- Set default to global indx space
    -- then check to see if a preferred tablespace has been set.
    SELECT COUNT(1) INTO i_count FROM USER_TABLESPACES WHERE TABLESPACE_NAME='INDX';
    IF i_count>0 THEN
      SELECT TABLESPACE_NAME INTO v_indexspace FROM USER_TABLESPACES WHERE TABLESPACE_NAME='INDX';
    END IF;
    -- Preferred tablespaces are configured using SetGOOMIndexSpace which puts the preferred
    -- index tablespace in GDOSYS.GPARAMETERS.
    SELECT COUNT(1) INTO i_count FROM GDOSYS.GPARAMETERS WHERE GPARAMETER='GOOMIndexSpace';
    IF i_count>0 THEN
      SELECT GVALUE INTO v_indexspace FROM GDOSYS.GPARAMETERS WHERE GPARAMETER='GOOMIndexSpace';
    END IF;
    -- Return the tablespace to use.
    RETURN v_indexspace;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, v_indexspace,'NONE',sqlcode,sqlerrm);
  END GetGOOMIndexSpace;
  ---------------------------------------------------------------------------------
  -- Sets the default spatial indexing tablespace.
  -- Syntax: EXEC GOOM.SetGOOMIndexSpace(v_indexspace);   
  --         Set v_indexspace to 'D' to revert to system default of USERS or INDX
  PROCEDURE SetGOOMIndexSpace( v_indexspace IN VARCHAR2) AS
    --
    c_cmdname       CONSTANT VARCHAR2(17) := 'SetGOOMIndexSpace';
    c_feedback1     CONSTANT VARCHAR2(40) := 'Default Index tablespace entry deleted.';
    c_feedback2     CONSTANT VARCHAR2(26) := 'Index tablespace set to: ';
    --
    v_chk1          PLS_INTEGER;
    v_chk2          PLS_INTEGER;
    v_sql           VARCHAR2(255);
    v_indxspace     USER_TABLESPACES.TABLESPACE_NAME%TYPE;
  BEGIN
    v_indxspace:=SUBSTR(UPPER(v_indexspace),1,30);  -- Make sure indexspace is UPPERCASE and 30 char.
    --Check for existing entry for default tablespace.
    SELECT COUNT(1) INTO v_chk1 FROM GDOSYS.GPARAMETERS WHERE GPARAMETER='GOOMIndexSpace';
    SELECT COUNT(1) INTO v_chk2 FROM USER_TABLESPACES   WHERE TABLESPACE_NAME=v_indxspace;
    -- If indexspace exists, change it, otherwise insert new one.
    IF v_chk1 = 0 AND v_chk2 != 0 THEN 
       INSERT INTO GDOSYS.GPARAMETERS VALUES('GOOMIndexSpace', v_indxspace);
       COMMIT;
       Response( c_cmdname, c_feedback2 || v_indxspace);
    ELSIF v_chk1 = 0 AND v_chk2 = 0 THEN -- specified tablespace does not exist;
       IF v_indxspace = 'D' THEN
          NULL;
       ELSE
          RAISE e_TabSpaceNotFound;   
       END IF;
    ELSIF v_chk1 != 0 and v_chk2 = 0 THEN
       IF v_indxspace = 'D' THEN
          v_sql:='DELETE FROM GDOSYS.GPARAMETERS WHERE GPARAMETER=''GOOMIndexSpace''';
          EXECUTE IMMEDIATE v_sql;
          COMMIT;
          Response( c_cmdname, c_feedback1);
          Response( c_cmdname, c_feedback2||GetGOOMIndexSpace);
        ELSE
          RAISE e_TabSpaceNotFound;
        END IF;
     ELSIF v_chk1 != 0 and v_chk2 != 0 THEN
        v_sql:='UPDATE GDOSYS.GPARAMETERS SET GVALUE=:vindexspace WHERE GPARAMETER=''GOOMIndexSpace''';
        EXECUTE IMMEDIATE v_sql USING v_indxspace;
        COMMIT;
        Response( c_cmdname, c_feedback2 || v_indxspace);  
      END IF;    
  EXCEPTION
    WHEN e_TabSpaceNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTabSpaceNotFound || v_indxspace);
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, v_sql, v_indxspace, sqlcode, sqlerrm);
      ROLLBACK;
  END SetGOOMIndexSpace;
  --
  -- -----------------------------------------------------------------------------------------------------------------
  -- UTILITY FUNCTIONS: Useful utility functions.
  -- -----------------------------------------------------------------------------------------------------------------
  --
  -- Convert an Integer value to a base value.  For example, binary is base 2:
  -- Example: Select GOOM.Integer2Base(666,2) FROM DUAL;
  FUNCTION Integer2Base( i_int IN NUMBER, n_base IN NUMBER ) RETURN VARCHAR2 IS
    c_cmdname   VARCHAR2(13) := 'Integer2Base';
    --
    v_STR	    VARCHAR2(255) DEFAULT NULL;
    v_NUM	    NUMBER	      DEFAULT i_int;
    v_HEX	    VARCHAR2(32)  DEFAULT '0123456789ABCDEF';
    c_err1      VARCHAR2(25)  DEFAULT 'Value must be integer > 0';
  --
  BEGIN
  	IF ( i_int IS NULL OR n_base IS NULL ) 
  	THEN
  		RETURN NULL;
  	END IF;
  	IF ( TRUNC( i_int ) <> i_int OR i_int < 0 ) THEN
  		RAISE e_invalidInput;
  	END IF;
  	LOOP
        v_STR := SUBSTR( v_HEX, MOD( v_NUM, n_base )+1, 1 ) || v_STR;
        v_NUM := TRUNC( v_NUM / n_base );
  		EXIT WHEN ( v_NUM = 0 );
  	END LOOP;
  	RETURN v_STR;
  EXCEPTION
    WHEN e_InvalidInput THEN
       Response ( c_cmdname || c_msgError, c_msgInvalidInput || c_err1 );
    WHEN OTHERS THEN
       GOOM.REPORT_ERROR ( c_cmdname, i_int, n_base, sqlcode, sqlerrm);
  END Integer2Base;
  -- ------------------------------------------------------------------------------------
  -- Convert a base value to a decimal value.  For example, binary is base 2:
  -- Example: Select GOOM.Base2Integer('001010011010',2) FROM DUAL;
  FUNCTION Base2Integer( v_str IN VARCHAR2,  n_base IN NUMBER DEFAULT 2 ) RETURN NUMBER IS
    c_cmdname   VARCHAR2(13):='Base2Integer';
    --
    v_num       NUMBER          DEFAULT 0;
    v_hex       VARCHAR2(32)    DEFAULT '0123456789ABCDEF';
  BEGIN
  	IF ( v_str IS NULL OR n_base IS NULL )
  	THEN
  		RETURN NULL;
  	END IF;
  	FOR i IN 1 .. LENGTH( v_str ) 
      LOOP
        v_num := v_num * n_base + instr( v_hex, upper(substr( v_str, i, 1 ))) - 1;
  	END LOOP;
  	RETURN v_num;
    EXCEPTION
      WHEN OTHERS THEN
         GOOM.REPORT_ERROR( c_cmdname, v_str, n_base, sqlcode, sqlerrm);
  END Base2Integer;
  ---------------------------------------------------------------------------------
  -- Convert an integer to hexadecimal.
  -- Syntax: v_hex:=GOOM.Dec2Hex(i_integer);
  FUNCTION DEC2HEX( i_integer IN INTEGER) RETURN VARCHAR2 is
  BEGIN
    RETURN Integer2Base( i_integer,16);
  END  DEC2HEX;
  ---------------------------------------------------------------------------------
  -- Convert an integer to binary.
  -- Syntax: v_binary:=GOOM.Num2Binary( i_integer );
  FUNCTION Num2Binary ( v_number in NUMBER ) RETURN VARCHAR2 IS
    c_cmdname     CONSTANT VARCHAR2(32) := 'Num2Binary'; 
    --   
    v_num         NUMBER := ROUND(ABS(v_number),0);
    v_binary      VARCHAR2(64);
  BEGIN
   IF v_number <> 1 THEN
    WHILE ( v_num > 0 ) LOOP
      v_binary := mod( v_num, 2)|| v_binary;
      v_num    := trunc( v_num / 2 );
    END LOOP;
    v_binary := PadBinary( v_binary );
   ELSE
    v_binary := '00000001';
   END IF;
    RETURN v_binary;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_num, v_binary, sqlcode, sqlerrm);
  END Num2Binary;
  ---------------------------------------------------------------------------------
  -- Convert a binary to an integer.
  -- Syntax: v_binary:=GOOM.Num2Binary( i_integer );
  FUNCTION Binary2Num ( v_binary in VARCHAR2 ) RETURN NUMBER IS
  c_cmdname         CONSTANT VARCHAR2(32) := 'Binary2Num';  
  --
  v_binlength       NUMBER;
  v_number          NUMBER := 0;
  v_curdigit        CHAR(1);
  v_curdec          NUMBER;
  v_debug           VARCHAR2(32):='Init';
  BEGIN
    v_binlength := LENGTH( v_binary );
    FOR i in 1..v_binlength loop
       v_curdigit := SUBSTR( v_binary, i, 1);
       v_curdec   := TO_NUMBER( v_curdigit );
       v_number   := ( v_number * 2 ) + v_curdec;
       v_debug    := 'In Loop: '|| i;
    END LOOP;
    RETURN v_number;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_binary, v_debug, sqlcode, sqlerrm);
      RETURN 0;
  END Binary2Num;
  ---------------------------------------------------------------------------------
  -- Pad binary output to multiples of 8 bits (1 byte)
  -- Syntax: v_binary:=GOOM.PadBinary( v_binary );
  Function PadBinary( v_binary IN VARCHAR2 ) RETURN VARCHAR2 IS
  c_cmdname CONSTANT VARCHAR2(32) := 'PadBinary'; 
  --
  v_mod     PLS_INTEGER;
  v_pad     VARCHAR2(7);
  v_padbin  VARCHAR2(64);
  BEGIN  
    v_mod := MOD(LENGTH( v_binary ),8);
    IF v_mod <> 0 THEN 
      CASE v_mod
           WHEN 1 THEN v_pad := '0000000';
           WHEN 2 THEN v_pad := '000000';
           WHEN 3 THEN v_pad := '00000';
           WHEN 4 THEN v_pad := '0000';
           WHEN 5 THEN v_pad := '000';
           WHEN 6 THEN v_pad := '00';
           WHEN 7 THEN v_pad := '0';
           ELSE v_pad := '';
      END CASE;
    END IF;
    RETURN v_pad || v_binary;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_binary, v_pad, sqlcode, sqlerrm);
  END PadBinary;
  ---------------------------------------------------------------------------------
  -- RBitShift returns the Right BitShift value of the input integer based on the shift.
  -- Syntax: v_hex:=GOOM.RBitShift(i_integer,2);
  FUNCTION RBitShift( i_val IN INTEGER, i_Shift IN INTEGER) RETURN INTEGER IS
  BEGIN
	  Return FLOOR( i_val / power(2, i_shift));
  END;
  -- LBitShift returns the Left BitShift value of the input integer based on the shift.
  -- Syntax: v_hex:=GOOM.LBitShift(i_integer,2);
  FUNCTION LBitShift( i_val IN INTEGER, i_Shift IN INTEGER) RETURN INTEGER IS
  BEGIN
        RETURN TO_NUMBER(SUBSTR(TO_CHAR(Integer2Base( i_val, 2)),( i_shift ) + 1));
  END;
  ---------------------------------------------------------------------------------
  -- GetGMTextFormat returns a text string describing the GM text format stored in
  -- Oracle's sdo_geometry.  The return includes the type and the alignment.
  -- Syntax: select GetGMTextFormat (v_value ) from dual;
  FUNCTION GetGMTextFormat ( v_value in INTEGER) RETURN VARCHAR2 IS
    c_cmdname     CONSTANT VARCHAR2(32) := 'GetGMTextFormat'; 
    --
    v_return      VARCHAR2(128)         := 'Nothing to return';
    v_align       PLS_INTEGER;          -- Alignment value (0 - 10 )
    v_type        PLS_INTEGER;          -- Format Type value 0,1,2
    v_alignment   VARCHAR2(64);         -- Alignment Text from GDO
    v_format      VARCHAR2(64);         -- Format Text from GDO
  BEGIN
    -- Check if value is 1, this is old Multi-byte Text reference.
    IF v_value = 1 THEN
      v_return := 'Multi-byte Text';
    ELSE
      -- Extract Alignment (1st byte) and Format (2nd byte)
      v_align := NVL(Binary2Num(SUBSTR(NUM2BINARY( v_value ) ,1,8)),1);
      v_type  := NVL(Binary2Num(SUBSTR(NUM2BINARY( v_value ) ,9,8)),0);
      -- Assign Alignment based on value
      CASE v_align
           WHEN '0' THEN v_alignment  := 'Center Center';
           WHEN '1' THEN v_alignment  := 'Center Left';
           WHEN '2' THEN v_alignment  := 'Center Right';
           WHEN '4' THEN v_alignment  := 'Top Center';
           WHEN '5' THEN v_alignment  := 'Top Left';
           WHEN '6' THEN v_alignment  := 'Top Right';
           WHEN '8' THEN v_alignment  := 'Bottom Center';
           WHEN '9' THEN v_alignment  := 'Bottom Left';
           WHEN '10' THEN v_alignment := 'Bottom Right';
           ELSE v_alignment := '**Invalid Alignment: '|| v_align ||' **';
      END CASE;
      --Assign type based on value.
      CASE v_type
           WHEN '0' THEN v_format  := 'Rich Text (RTF)';
           WHEN '1' THEN v_format  := 'Multi-byte Text';
           WHEN '2' THEN v_format  := 'Unicode Text';
           ELSE v_format := '**Invalid Format: ' || v_type ||' **';
      END CASE;
      v_return := v_format ||' aligned '|| v_alignment ||'.';
    END IF;
    RETURN v_return;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_return, v_align ||':'|| v_type, sqlcode, sqlerrm);
  END GetGMTextFormat;
  ---------------------------------------------------------------------------------
  -- RandInRange returns a random real number between the integer values i_lo and i_hi.
  -- Syntax: v_value:=GOOM.RandInRange(i_lo, i_hi);
  FUNCTION RandInRange( i_lo IN INTEGER, i_hi IN INTEGER) RETURN NUMBER IS
    c_cmdname CONSTANT VARCHAR2(32) := 'RandInRange';
    n_num     NUMBER;
  BEGIN
    n_num := TRUNC(ABS((GREATEST( i_lo, i_hi ) - LEAST( i_lo, i_hi ) + 1) * ABS(DBMS_RANDOM.VALUE)) + LEAST( i_lo, i_hi ));
    RETURN n_num;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, 'Low:' || i_lo || ' High:' || i_hi, 'Random Number: ' || n_num, SQLCODE, SQLERRM);
  END RandInRange;
  ---------------------------------------------------------------------------------
  -- Text2Integer returns an integer value from 4 bytes of text.  Anything more will be truncated.
  -- Syntax: i_integer:=GOOM.Text2Integer(c_string);
  FUNCTION Text2Integer( v_string IN VARCHAR2 ) RETURN INTEGER IS
    c_cmdname   CONSTANT VARCHAR2(12):='Text2Integer';
    v_text      VARCHAR2(4);
    v_hex       VARCHAR2(16):=NULL;
    i_integer   INTEGER;
    v_strlen    INTEGER;
  BEGIN
    v_text  :=RPAD(SUBSTR( v_string, 1, 4), 4);
    v_strlen:=LENGTH( v_text );
    FOR i in 0 .. v_strlen-1 LOOP
      v_hex:= v_hex || Integer2Base(ASCII(SUBSTR( v_text, v_strlen-i,1)),16);
    END LOOP;
    i_integer:=TO_NUMBER( v_hex, RPAD('X',LENGTH( v_hex ),'X'));
  RETURN i_integer;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_text , i_integer, v_hex, SQLERRM );    
      RETURN 0;
  END Text2Integer;
  ---------------------------------------------------------------------------------
  -- String2Integers returns a comma delimited string of integers, every 4 bytes (incl. spaces) is one integer.
  -- Syntax: i_integerList:=GOOM.String2Integers(c_string);
  FUNCTION String2Integers( v_string IN VARCHAR2 ) RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(20) := 'String2Integers';
    --
    v_text          VARCHAR2(4);
    v_strlen        INTEGER;
    v_numords       INTEGER;
    v_ord           INTEGER;
    v_ordinates     VARCHAR2(2048);
  BEGIN
    v_strlen  := LENGTH(v_string);
  	IF REMAINDER( v_strlen, 4) = 0 THEN
  	  v_numords := v_strlen/4;
  	ELSE
  	  v_numords := FLOOR( v_strlen/4) +1;	
  	END IF;
    FOR i IN 1 .. v_numords
    LOOP
  	  v_ord  := i*4;
      v_text := RTRIM(SUBSTR( v_string, v_ord-3, 4));
      IF i = 1 THEN
        v_ordinates := TO_CHAR(GOOM.Text2Integer( v_text ));
  	  ELSE 
  	    v_ordinates := v_ordinates || ',' || TO_CHAR(Text2Integer( v_text ));
      END IF;
    END LOOP;
    RETURN v_strlen||','|| v_ordinates;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ord, v_ordinates, v_text, SQLERRM );
      RETURN NULL;
  END String2Integers;
  ---------------------------------------------------------------------------------
  -- Integer2Text returns 4 bytes of text from the input integer.
  -- Syntax: v_text:=GOOM.Integer2Text(i_integer);
  FUNCTION Integer2Text( i_integer IN INTEGER ) RETURN VARCHAR2 IS
    c_cmdname     CONSTANT VARCHAR2(12):='Integer2Text';
    --
    v_hex         VARCHAR2(2);
    v_chr         VARCHAR2(4):=NULL;
  BEGIN
    FOR i IN 1 .. 4 LOOP
      v_hex := SUBSTR(LTRIM(TO_CHAR( i_integer, 'xxxxxxxx')),9-(2*i),2);
      v_chr := v_chr||CHR(TO_NUMBER( v_hex, RPAD('X',LENGTH( v_hex ),'X')));
    END LOOP;
    RETURN v_chr;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, i_integer, v_hex, SQLCODE, SQLERRM);    
      RETURN NULL;
  END Integer2Text;
  ---------------------------------------------------------------------------------
  -- Integers2String returns a text string from a comma delimited string of integers.
  -- Syntax: v_string:=GOOM.Integers2String(c_integerList);  
  FUNCTION Integers2String( v_string IN VARCHAR2 ) RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(16) :='Integers2String';
    --
    i_integer       INTEGER;
    v_length        INTEGER;
    i_intlen        INTEGER;
    v_strlen        INTEGER;
    v_start         INTEGER;
    v_numords       INTEGER;
    v_text          VARCHAR2(2048);
  BEGIN
    v_length := LENGTH( v_string);
    v_strlen := TO_NUMBER(SUBSTR( v_string, 1, INSTR( v_string,',',1)-1));
  	IF REMAINDER( v_strlen,4) = 0 THEN
  	  v_numords := v_strlen/4;
  	ELSE
  	  v_numords := FLOOR( v_strlen/4) +1;	
  	END IF;
    FOR i IN 1 .. v_numords
    LOOP
      v_start := INSTR( v_string,',',1,i)+1;
      IF i <> v_numords THEN
        i_intlen := INSTR( v_string,',',1,i+1) - v_start;
      ELSE
        i_intlen := v_length - v_start +1;
      END IF;
        i_integer := TO_NUMBER(SUBSTR( v_string, v_start, i_intlen));
      IF i = 1 THEN
        v_text := RPAD(Integer2Text( i_integer ), 4);
  	  ELSE 
  	    v_text := v_text || RPAD(Integer2Text( i_integer ), 4);
      END IF;
    END LOOP;
    RETURN v_text;
  EXCEPTION  
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_start, i_intlen, i_integer, SQLERRM);
      RETURN NULL;
  END Integers2String;
  ---------------------------------------------------------------------------------
  -- StartTime returns the database starting time in hsec
  -- Syntax: v_start_time:=GOOM.StartTime;
  --
  FUNCTION StartTime RETURN INTEGER IS
  BEGIN
    RETURN DBMS_UTILITY.GET_TIME;
  END StartTime;
  -- TotalTime returns the elapsed time in seconds, use with StartTime.
  -- Syntax: v_elapsed_time:=GOOM.TotalTime(v_start_time);
  --
  FUNCTION TotalTime ( i_start_time IN INTEGER ) RETURN NUMBER IS
    c_cmdname       CONSTANT VARCHAR2(9) := 'TotalTime';
    i_end_time      INTEGER := 0;
    e_noStartTime   EXCEPTION;
    c_msgNoStartTime VARCHAR2(32) := 'A valid start time is required.';
  BEGIN
    IF i_start_time IS NULL THEN
      RAISE e_noStartTime;
    END IF;
    i_end_time := DBMS_UTILITY.GET_TIME;
	RETURN ABS( i_end_time - i_start_time )/100;
  EXCEPTION
    WHEN e_noStartTime THEN
      Response( c_cmdname || c_msgError, c_msgNoStartTime);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, i_start_time, i_end_time, SQLCODE, SQLERRM );
  END TotalTime;
  ---------------------------------------------------------------------------------
  -- Bearing returns bearing between 2 points.
  -- Syntax: n_bearing := BEARING(x1,y1,x2,y2,v_ctype,v_rtype);
  --         v_ctype : Default 'P' for projected.  Otherwise calculates for long lat.
  --         v_rtype : Default 'D' for degrees.  Otherwise calculates radians.
  FUNCTION Bearing( n_xcoord1 IN NUMBER, n_ycoord1 IN NUMBER, n_xcoord2 IN NUMBER, n_ycoord2 IN NUMBER, 
                      v_ctype IN VARCHAR2 DEFAULT 'P', v_rtype in VARCHAR2 DEFAULT 'D') RETURN NUMBER IS
    c_cmdname   VARCHAR2(7) := 'Bearing';
    --
    v_azimuth   NUMBER :=0; 
    v_bearing   NUMBER :=0; 
    v_calc1     NUMBER :=0;
    v_calc2     NUMBER :=0;
  BEGIN
    IF v_ctype = 'P' THEN
      v_calc1 := n_xcoord2 - n_xcoord1; 
      v_calc2 := n_ycoord2 - n_ycoord1; 
    ELSE
      v_calc1 := sin( n_xcoord2 - n_xcoord1 ); 
      v_calc2 := cos( n_ycoord1 ) * tan( n_ycoord2 ) - sin( n_ycoord1 ) * cos( n_xcoord2 - n_xcoord1 ); 
    END IF;
    IF( v_calc2 = 0 ) THEN 
      IF( v_calc1 > 0 ) THEN 
        v_azimuth := PI/2; 
      ELSIF( v_calc1 < 0 ) THEN
        v_azimuth := -PI/2; 
      ELSE
        RAISE e_nullAzimuth;
      END IF; 
    ELSE 
      v_azimuth := atan( v_calc1 / v_calc2); 
    END IF; 
    IF( n_ycoord2 < n_ycoord1 ) THEN 
      v_bearing := v_azimuth + PI; 
    ELSE 
      v_bearing := v_azimuth; 
    END IF; 
    IF( v_bearing < 0 ) THEN 
      v_bearing := v_bearing + (2 * PI); 
    END IF; 
    IF v_rtype = 'D' THEN
      RETURN v_bearing * 180/PI;
    ELSE
      RETURN v_bearing;
    END IF;
  EXCEPTION
    WHEN e_NullAzimuth THEN
      REPORT_ERROR( c_cmdname,'Start Coords:'|| n_xcoord1 ||','|| n_ycoord1 ||' End Coords:'|| n_xcoord2 ||','|| n_ycoord2, c_msgNullAzimuth, SQLCODE, SQLERRM);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname,'Start Coords:'|| n_xcoord1 ||','|| n_ycoord1 ||' End Coords:'|| n_xcoord2 ||','|| n_ycoord2, c_msgOraError, SQLCODE, SQLERRM);
  END Bearing;
  ---------------------------------------------------------------------------------
  -- DMS2DD converts Degree:Minute:Seconds to Decimal Degrees.
  -- Syntax: n_dec_degrees := DMS2DD(v_dms);
  --         SELECT DMS2DD('40:20:50') FROM DUAL;
  --         v_dms : colon (:) delimited by default.
  FUNCTION DMS2DD ( v_dms IN VARCHAR2 ) Return NUMBER IS
  c_cmdname     VARCHAR2(6) :='DMS2DD';
  --
  v_dms_in      VARCHAR2(25):= '';
  v_sign        VARCHAR2(1) := '';
  n_dd          NUMBER      := 0;
  n_Degree_pos  NUMBER      := 0;
  n_Minute_pos  NUMBER      := 0;
  n_Degree      NUMBER      := 0;
  n_Minute      NUMBER      := 0;
  n_Second      NUMBER      := 0;
  n_Dec_Degree  NUMBER(10,7):= 123.456789;
  i             INT;
  --
  TYPE TextArray IS TABLE OF varchar(1) INDEX BY BINARY_INTEGER;
  objTextArray  TextArray;	        
  --  
  BEGIN
    v_dms_in  := trim( v_dms );
    -- Strip off the leading sign to be added to the final output.
    IF SUBSTR( v_dms_in, 1, 1) = '-' THEN
       v_sign   := '-';
       v_dms_in := SUBSTR( v_dms_in, 2);
    END IF;
    -- load the TEXT array.
    FOR i IN 1..LENGTH( v_dms_in ) LOOP
        objTextArray(i) := SUBSTR( v_dms_in, i, 1);
    END LOOP;
    -- Find the delimiters.
    FOR i IN 1..LENGTH( v_dms_in ) LOOP
      IF objTextArray(i) NOT IN ('1','2','3','4','5','6','7','8','9','0','.') THEN
        IF n_Degree_pos = 0 THEN
           n_Degree_pos := i;
        ELSE
           n_Minute_pos := i;
        END IF;
      END IF;
    END LOOP;
    IF n_Degree_pos > 0 AND n_Minute_pos > 0 THEN
       n_Degree := TO_NUMBER(substr( v_dms_in, 1, n_Degree_pos - 1));
       n_Minute := TO_NUMBER(substr( v_dms_in, n_Degree_pos + 1, n_Minute_pos - ( n_Degree_pos + 1)));
       n_Second := TO_NUMBER(substr( v_dms_in, n_Minute_pos + 1));
       n_Dec_Degree := n_Degree + ( n_Minute * (1/60)) + ( n_Second * ((1/60) * (1/60)));
       n_dd := v_sign || n_Dec_Degree;
    END IF;
    RETURN ( n_dd );
  EXCEPTION
    WHEN NO_DATA_FOUND Then
      n_dd := 0;
      Return ( n_dd );
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, 'Input:'|| v_dms, 'Output:'|| n_dd, SQLCODE, SQLERRM);
      n_dd := 0;
      Return ( n_dd );
  END DMS2DD;
  ---------------------------------------------------------------------------------
  -- DD2DMS converts Decimal Degrees to Lat or Lon: Degree, Minute, Seconds using delimiter.
  -- Syntax: v_dec_degrees := DMS2DD(v_dms,v_delim);
  --         v_dms := DD2DMS(n_dd,v_delim);
  --         SELECT DD2DMS(40.3472222,':') FROM DUAL;
  --         v_delim : any single character like ':'
  FUNCTION DD2DMS ( n_dd IN NUMBER, v_delim IN VARCHAR2 DEFAULT ':' ) RETURN VARCHAR2 IS
  c_cmdname         VARCHAR2(6)  := 'DD2DMS';
  --
  v_result          VARCHAR2(25) := '';
  v_sign            VARCHAR2(1)  := '';
  --
  n_Degree_floor    NUMBER := 0;
  n_Minute_floor    NUMBER := 0;
  n_Second_floor    NUMBER := 0;
  n_Degree          NUMBER := 0;
  n_Minute          NUMBER := 0;
  n_Second          NUMBER := 0;
  n_Second_Fraction NUMBER := 0;
  --
  Begin
    If SUBSTR( n_dd, 1, 1) = '-' then
       v_sign   := '-';
       n_Degree := SUBSTR( n_dd, 2 );
    ELSE
      n_Degree := n_dd;
    END IF;
    n_Degree_floor    := floor( n_Degree );
    n_Minute          := n_Degree - floor( n_Degree );
    n_Minute          := n_Minute * 60;
    n_Minute_floor    := floor( n_Minute );
    n_Second          := n_Minute - floor( n_Minute );
    n_Second          := n_Second * 60;
    n_Second_floor    := floor( n_Second );
    n_Second_Fraction := n_Second - floor( n_Second );
    v_result := v_sign || n_Degree_floor || v_delim || Lpad( n_Minute_floor, 2, '0') || v_delim || Lpad( n_Second_floor, 2, '0') || n_Second_fraction ;
    RETURN ( v_result );
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, 'Input:'|| n_dd, 'Output:' || v_result, SQLCODE, SQLERRM);
      RETURN ('0:0:0');
  END DD2DMS;
  ---------------------------------------------------------------------------------
  -- Ang2Rad converts the input degrees to radians.
  -- Syntax: n_radians := Ang2Rad(n_degrees);
  FUNCTION Ang2Rad ( n_degrees IN NUMBER ) RETURN NUMBER IS
  BEGIN
    RETURN n_degrees*(PI/180);
  END Ang2Rad;
  ---------------------------------------------------------------------------------
  -- Rad2Ang converts the input radians to degrees.
  -- Syntax: n_degrees := Rad2Ang(n_radians);
  FUNCTION Rad2Ang ( n_radians IN NUMBER) RETURN NUMBER IS
  BEGIN
    RETURN n_radians*(180/PI);
  END Rad2Ang;
  ---------------------------------------------------------------------------------
  -- ROTINDEX returns the rotation matirx index value for the input angle in degrees
  -- Syntax: n_irot := ROTINDEX('I',n_degrees);
  --         n_jrot := ROTINDEX('J',n_degrees);
  FUNCTION ROTINDEX ( v_type IN VARCHAR2, n_degrees IN NUMBER) RETURN NUMBER IS
    n_radians FLOAT;
  BEGIN
    n_radians:=Ang2Rad( n_degrees );
    IF UPPER( v_type ) ='I' THEN
      RETURN COS( n_radians );
    ELSE
      RETURN SIN( n_radians );
    END IF;
  END ROTINDEX;
  ---------------------------------------------------------------------------------
  -- ROTANGLE returns the rotation angle in degrees from the input i, j rotation indicies.
  -- Syntax: n_degrees := ROTANGLE(n_irot, n_jrot);
  FUNCTION ROTANGLE ( n_irot IN NUMBER, n_jrot IN NUMBER) RETURN NUMBER IS
  BEGIN
    RETURN ROUND(Rad2Ang(ATAN2( n_jrot, n_irot)), 3);
  END ROTANGLE;
  
  -- -----------------------------------------------------------------------------------------------------------------
  -- CHECK FUNCTIONS: Returns a BOOLEAN TRUE/FALSE based on condition.
  -- -----------------------------------------------------------------------------------------------------------------

  -- chkTable returns BOOLEAN TRUE if the tablename exists in the schema.
  -- Example: IF GOOM.ChkTable(v_tablename) THEN
  FUNCTION chkTable( v_tablename IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(10) := 'ChkTable';
    i_count         PLS_INTEGER;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);    
    v_table         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable :=GetOwnerObject( v_tablename );
    v_schema     :=SplitOwnerObject( v_ownertable,'OWNER' );
    v_table      :=SplitOwnerObject( v_ownertable,'TABLE' ); 
    --  Check Oracle's dictionary views for the table's existence.  
    v_sql:='SELECT COUNT(1) FROM ALL_TABLES WHERE OWNER = :vowner AND TABLE_NAME = :vtable';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_table;
    IF i_count > 0 THEN
      RETURN TRUE;  -- Table exists in schema indicated by owner
    ELSE
      RAISE e_TableNotFound;
    END IF;
  EXCEPTION
    WHEN e_TableNotFound THEN
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkTable;
  ----------------------------------------------------------------------------------
  -- chkColumn returns BOOLEAN TRUE if the column exists in the table.
  -- Syntax: IF GOOM.ChkColumn(v_tablename, v_colName) THEN
  FUNCTION chkColumn( v_tablename IN VARCHAR2, v_colName in VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(10) := 'chkColumn';
    i_count         PLS_INTEGER;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);    
    v_table         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_tablename);
    v_schema     :=SplitOwnerObject( v_ownertable,'OWNER');
    v_table     :=SplitOwnerObject( v_ownertable,'TABLE'); 
    --  Check Oracle's dictionary views for the table's existence.  
    IF chkTable( v_ownertable ) THEN 
      v_sql:='SELECT COUNT(1) FROM ALL_TAB_COLUMNS WHERE OWNER = :vowner AND TABLE_NAME = :vtable AND COLUMN_NAME = :vcolName';
      EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_table, v_colName;
      IF i_count > 0 THEN
        RETURN TRUE;  -- Column exists in table indicated by owner
      ELSE
        RAISE e_ColumnNotFound;
      END IF;
    ELSE
      RAISE e_TableNotFound;
    END IF;
  EXCEPTION
    WHEN e_ColumnNotFound THEN
      RETURN FALSE;
    WHEN e_TableNotFound THEN
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkColumn;
  ----------------------------------------------------------------------------------
  -- ChkTableData returns BOOLEAN TRUE if the table or view contains data.
  -- Syntax: IF GOOM.ChkTableData(v_tablename) THEN
  FUNCTION chkTableData( v_tablename IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(12) := 'chkTableData';
    v_ownertable    VARCHAR2(61);
  BEGIN
    v_ownertable:=GetOwnerObject(v_tablename);
    IF NOT ChkTable( v_ownertable ) AND NOT ChkView( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    IF GetCount( v_ownertable ) > 0 THEN
      RETURN TRUE;  -- Table or view contains data
    ELSE
      RETURN FALSE; -- Table or view does not contain data
    END IF;
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgTableNotFound || v_ownertable);
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, c_msgNone, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkTableData;
  -----------------------------------------------------------------------------------
  -- ChkView returns BOOLEAN TRUE if the indicated tablename is a view.
  -- Syntax: IF GOOM.ChkView(v_tablename) THEN
  FUNCTION chkView( v_viewname IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(7) := 'ChkVIEW';
    i_count         PLS_INTEGER;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownerview     VARCHAR2(61);
    v_schema         VARCHAR2(30);    
    v_view          VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownerview :=GetOwnerObject( v_viewname );
    v_schema     :=SplitOwnerObject( v_ownerview, 'OWNER');
    v_view      :=SplitOwnerObject( v_ownerview, 'TABLE');
    --
    v_sql:='SELECT COUNT(1) FROM ALL_VIEWS WHERE OWNER = :vowner AND VIEW_NAME = :vview';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_view;
    IF i_count > 0 THEN
      RETURN TRUE;  -- Table is a view
    ELSE
      RAISE e_ObjNotFound; -- Table is not a view
    END IF;
  EXCEPTION
    WHEN e_ObjNotFound THEN
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownerview, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkView;
  -----------------------------------------------------------------------------------
  -- ChkMView returns BOOLEAN TRUE if the indicated tablename is a materialized view.
  -- Syntax: IF GOOM.ChkMView(v_tablename) THEN
  FUNCTION chkMView( v_viewname IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(7) := 'ChkVIEW';
    i_count         PLS_INTEGER;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownerview     VARCHAR2(61);
    v_schema         VARCHAR2(30);    
    v_view          VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownerview :=GetOwnerObject( v_viewname );
    v_schema     :=SplitOwnerObject( v_ownerview, 'OWNER');
    v_view      :=SplitOwnerObject( v_ownerview, 'TABLE');
    --
    v_sql:='SELECT COUNT(1) FROM ALL_MVIEWS WHERE OWNER = :vowner AND MVIEW_NAME = :vview';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_view;
    IF i_count > 0 THEN
      RETURN TRUE;  -- Table is a materialized view
    ELSE
      RAISE e_ObjNotFound;
    END IF;
  EXCEPTION
    WHEN e_ObjNotFound THEN
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownerview, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkMView;
  -----------------------------------------------------------------------------------
  -- ChkSequence returns BOOLEAN TRUE if the sequence exists in the schema.
  -- Syntax: IF GOOM.ChkSequence(v_seqname) THEN 
  FUNCTION chkSequence( v_seqname IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(12) := 'ChkSequence'; 
    --
    i_count         PLS_INTEGER;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownerseq      VARCHAR2(61);
    v_schema        VARCHAR2(30);    
    v_seq           VARCHAR2(30); 
  BEGIN
    -- added for owner.table support
    v_ownerseq  :=GetOwnerObject(v_seqname);
    v_schema     :=SplitOwnerObject(v_ownerseq,'OWNER');
    v_seq       :=SplitOwnerObject(v_ownerseq,'TABLE'); 
    -- Does the sequence exist?
    v_sql:='SELECT COUNT(1) FROM ALL_SEQUENCES WHERE SEQUENCE_NAME = :vseqname AND SEQUENCE_OWNER = :vowner';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_seq, v_schema;
    IF i_count > 0 THEN
      RETURN TRUE; -- Sequence exists in schema
    ELSE
      RAISE e_ObjNotFound; -- Sequence does not exist in schema
    END IF;
  EXCEPTION
    WHEN e_ObjNotFound THEN
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownerseq, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkSequence;
  ----------------------------------------------------------------------------------
  -- ChkTrigger returns BOOLEAN TRUE if the trigger exists in the schema.
  -- Syntax: IF GOOM.ChkTrigger(v_tablename) THEN
  FUNCTION chkTrigger( v_trigname IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(10) := 'ChkTrigger';   
    i_count         PLS_INTEGER;
    v_sql           VARCHAR2(512);
    -- added for owner.table support
    v_ownertrig     VARCHAR2(61);
    v_schema        VARCHAR2(30);    
    v_trig          VARCHAR2(30);
  BEGIN
    -- added for owner.trig support
    v_ownertrig :=GetOwnerObject( v_trigname);
    v_schema     :=SplitOwnerObject( v_ownertrig, 'OWNER');
    v_trig      :=SplitOwnerObject( v_ownertrig, 'TABLE'); 
    -- Does the trigger exist?
    v_sql:='SELECT COUNT(1) FROM ALL_TRIGGERS WHERE OWNER = :vowner AND TRIGGER_NAME = :vtrig';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_trig;
    IF i_count > 0 THEN
      RETURN TRUE;  -- Trigger exists in schema
    ELSE
      RAISE e_ObjNotFound; -- Trigger does not exist in schema
    END IF;
  EXCEPTION
    WHEN e_ObjNotFound THEN
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertrig, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkTrigger;
  ----------------------------------------------------------------------------------
  -- chkIndex returns BOOLEAN TRUE if the index exists.
  -- Syntax: IF GOOM.chkIndex(v_indexname) THEN
  FUNCTION chkIndex( v_indexname IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(8) := 'chkIndex'; 
    i_count         PLS_INTEGER := 0;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownerindex    VARCHAR2(61);
    v_schema        VARCHAR2(30);    
    v_index         VARCHAR2(30); 
  BEGIN
    -- added for owner.table support
    v_ownerindex :=GetOwnerObject( v_indexname);
    v_schema     :=SplitOwnerObject( v_ownerindex,'OWNER');
    v_index      :=SplitOwnerObject( v_ownerindex,'TABLE'); 
    --
    v_sql:='SELECT COUNT(1) FROM ALL_INDEXES WHERE OWNER = :vowner AND INDEX_NAME = :vindex';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_index;
    IF i_count > 0 THEN
      RETURN TRUE; -- Index exists
    ELSE
      RAISE e_ObjNotFound; -- Index does not exist.
    END IF;
  EXCEPTION
    WHEN e_ObjNotFound THEN
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownerindex, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkIndex;
  -----------------------------------------------------------------------------------
  -- chkSpatialIndex returns BOOLEAN TRUE if the spatial index already exists on the 
  -- specified table/geometry column.
  -- Syntax: IF GOOM.chkSptialIndex(v_tablename,v_geomcol) THEN
  FUNCTION chkSpatialIndex( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(15):='chkSpatialIndex';
    i_count         PLS_INTEGER;
    i_count1        PLS_INTEGER;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);
    v_table         VARCHAR2(30);
    v_geom          VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_tablename);
    v_schema     :=SplitOwnerObject( v_ownertable,'OWNER');
    v_table     :=SplitOwnerObject( v_ownertable,'TABLE');
    -- Get geometry if none specified
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable);
    ELSE
      v_geom := v_geomcol;
    END IF;
    -- Check that geometry exists
    IF v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    -- Check that spatial index exists.
    v_sql:='SELECT COUNT(1) FROM ALL_SDO_INDEX_INFO WHERE TABLE_OWNER=:vowner AND TABLE_NAME=:vtab AND COLUMN_NAME=:vcol';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_table, v_geom;
    IF i_count > 0 THEN
      RETURN TRUE;
    ELSE
      v_sql:='SELECT COUNT(1) FROM ALL_INDEXES WHERE TABLE_OWNER=:vowner AND TABLE_NAME=:vtab AND DOMIDX_OPSTATUS=''FAILED''';
      EXECUTE IMMEDIATE v_sql INTO i_count1 USING v_schema, v_table;
      IF i_count1 >0 THEN
        RETURN TRUE;
      ELSE
        RAISE e_ObjNotFound;
      END IF;
    END IF;
  EXCEPTION
    WHEN e_ObjNotFound THEN
      RETURN FALSE;
    WHEN e_GeometryNotFound THEN
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geomcol, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkSpatialIndex;
  -----------------------------------------------------------------------------------
  -- chkViewIndex is an internal only function to detect existence of index name in GINDEX_COLUMNS.
  -- Syntax: IF chkViewIndex(v_indexname) THEN
  FUNCTION chkViewIndex( v_viewindex IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'chkViewIndex';   
    i_count         PLS_INTEGER := 0;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownerindex    VARCHAR2(61);
    v_schema        VARCHAR2(30);    
    v_index         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownerindex:=GetOwnerObject( v_viewindex);
    v_schema    :=SplitOwnerObject( v_ownerindex,'OWNER');
    v_index     :=SplitOwnerObject( v_ownerindex,'TABLE');  
    -- 
    v_sql:='SELECT COUNT(1) FROM GDOSYS.GINDEX_COLUMNS WHERE OWNER = :vowner AND INDEX_NAME = :vindex';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_index;
    IF i_count > 0 THEN
      RETURN TRUE;  -- Index exists
    ELSE
      RAISE e_ObjNotFound; -- Index does not exist.
    END IF;
  EXCEPTION
    WHEN e_ObjNotFound THEN
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownerindex, 'Count: ' || i_count, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkViewIndex;
  ----------------------------------------------------------------------------------
  -- chkPKEY returns BOOLEAN TRUE if the column specified is a primary key for the table.
  -- Syntax: IF GOOM.chkPKEY(v_tablename,v_column) THEN
  FUNCTION chkPKEY( v_tablename IN VARCHAR2, v_column IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(7) := 'ChkPKEY'; 
    v_test1         PLS_INTEGER := 0;
    v_test2         PLS_INTEGER := 0;
    v_sql           VARCHAR2(512);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);    
    v_table         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_tablename);
    v_schema     :=SplitOwnerObject( v_ownertable,'OWNER');
    v_table     :=SplitOwnerObject( v_ownertable,'TABLE'); 
    -- Test 1:  Look for the primary key constraint by constraint name.
    v_sql:='SELECT COUNT(1) FROM ALL_IND_COLUMNS WHERE INDEX_OWNER = :vowner AND COLUMN_NAME = :vcolumn AND INDEX_NAME IN 
           (SELECT CONSTRAINT_NAME FROM ALL_CONSTRAINTS WHERE OWNER = :vowner AND CONSTRAINT_TYPE = ''P'' AND TABLE_NAME = :vtable)';
    EXECUTE IMMEDIATE v_sql INTO v_test1 USING v_schema, v_column, v_schema, v_table;
    -- Test 2:  Look for the primary key constraint by index name (from Gary Whitt).
    v_sql:='SELECT COUNT(1) FROM ALL_IND_COLUMNS WHERE INDEX_OWNER = :vowner  AND COLUMN_NAME = :vcolumn AND INDEX_NAME IN 
           (SELECT INDEX_NAME FROM ALL_CONSTRAINTS WHERE OWNER = :vowner AND CONSTRAINT_TYPE = ''P'' AND TABLE_NAME = :vtable)';
    EXECUTE IMMEDIATE v_sql INTO v_test2 USING v_schema, v_column, v_schema, v_table;
    -- If either test returns a value then we have a key else we do not.
    IF v_test1 > 0 OR v_test2 >0 THEN
      RETURN TRUE;  -- Column is a key
    ELSE
      RETURN FALSE; -- Column is not a key
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable ||'.'|| v_column, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END ChkPKEY;
  ----------------------------------------------------------------------------------
  -- ChkGeometry returns BOOLEAN TRUE if the indicated column is sdo_geometry or sdo_georaster.
  -- Syntax: IF GOOM.ChkGeometry(c_tablename, c_columnname) THEN
  FUNCTION chkGeometry( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(11) := 'chkGeometry';
    v_type          VARCHAR2(30);
    i_count         PLS_INTEGER;
    v_sql           VARCHAR2(255);
    v_column        VARCHAR2(62);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);
    v_table         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable :=GetOwnerObject( v_tablename);
    v_schema      :=SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      :=SplitOwnerObject( v_ownertable, 'TABLE');
    -- Check for GeoRaster
    IF INSTR( v_geomcol,'.')=0 THEN
      v_column := v_geomcol;
    ELSE
      v_column := SplitOwnerObject( v_geomcol, 'OWNER');
    END IF;
    -- End GeoRaster Check
    v_sql:='SELECT COUNT(1) FROM ALL_TAB_COLUMNS WHERE OWNER=:vowner AND TABLE_NAME = :vtablename AND COLUMN_NAME= :columnname';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_table, v_column;
    IF i_count = 0 THEN
      RETURN FALSE;
    END IF;
    v_sql:='SELECT DATA_TYPE FROM ALL_TAB_COLUMNS WHERE OWNER=:vowner AND TABLE_NAME = :vtablename AND COLUMN_NAME= :columnname';
    EXECUTE IMMEDIATE v_sql INTO v_type USING v_schema, v_table, v_column;
    IF v_type = 'SDO_GEOMETRY' OR v_type = 'SDO_GEORASTER' THEN
      RETURN TRUE;  -- spatial column
    ELSE
      RETURN FALSE; -- not a spatial column
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable ||'.'|| v_column, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkGeometry;
  ----------------------------------------------------------------------------------
  -- ChkTablePartition returns BOOLEAN TRUE if the table is partitioned by Range.
  -- Syntax: IF GOOM.chkTablePartition(c_tablename) THEN
  FUNCTION chkTablePartition( v_tablename IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(17) := 'ChkTablePartition';
    v_partchk       VARCHAR2(4);
    v_parttyp       VARCHAR2(9);
    v_sql           VARCHAR2(255);
    v_debug         VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);
    v_table         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_tablename);
    v_schema     :=SplitOwnerObject( v_ownertable,'OWNER');
    v_table     :=SplitOwnerObject( v_ownertable,'TABLE');
    --
    IF chkTable( v_ownertable) THEN
      v_sql:='SELECT PARTITIONED FROM ALL_TABLES WHERE OWNER = :vowner AND TABLE_NAME = :vtable';
      EXECUTE IMMEDIATE v_sql INTO v_partchk USING v_schema, v_table;
      IF v_partchk='YES' THEN
        v_sql:='SELECT PARTITIONING_TYPE FROM ALL_PART_TABLES WHERE OWNER = :vowner AND TABLE_NAME = :vtable';
        EXECUTE IMMEDIATE v_sql INTO v_parttyp USING v_schema, v_table;
        IF v_parttyp = 'HASH' THEN
          RETURN FALSE; -- Since we cannot do much with HASH partitioning, we treat as false.
        ELSE
          RETURN TRUE;  -- Range partitions can be spatially indexed so we treat as true.
        END IF;
      ELSE
        RETURN FALSE;   -- Table is not partitioned.
      END IF;
    ELSE
      v_debug:='Table does not Exist';
      RETURN FALSE;
    END IF;
    EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, v_debug, SQLCODE, SQLERRM);
      RETURN FALSE;
  END chkTablePartition;
  ----------------------------------------------------------------------------------
  -- ChkMetadata returns BOOLEAN TRUE if GDOSYS metadata exists for the specified table.
  -- Syntax: IF GOOM.ChkMetadata(c_tablename) THEN
  FUNCTION chkMetadata( v_tablename IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(11) := 'chkMetadata';
    --
    i_count1        PLS_INTEGER := 0;
    i_count2        PLS_INTEGER := 0;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject(v_tablename);
    --
    v_sql:='SELECT COUNT(1) FROM GDOSYS.GFEATURESBASE WHERE FEATURENAME = :vfeaturename';
    EXECUTE IMMEDIATE v_sql INTO i_count1 USING v_ownertable;
    v_sql:='SELECT COUNT(1) FROM GDOSYS.FIELDLOOKUP WHERE FEATURENAME = :vfeaturename';
    EXECUTE IMMEDIATE v_sql INTO i_count2 USING v_ownertable;
    IF i_count1 >= 1 OR i_count2 >= 1 THEN
      RETURN TRUE; -- Metadata exists
    ELSE
      RETURN FALSE; -- Metadata does not exist or is incomplete.
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, 'Test1: ' || i_count1, 'Test2: ' || i_count2, SQLERRM);
      RETURN FALSE;
  END chkMetadata;
  ----------------------------------------------------------------------------------
  -- ChkMBR returns BOOLEAN TRUE if Oracle metadata exists for the specified feature.
  -- Syntax: IF GOOM.ChkMBR(c_tablename,c_geomcol) THEN
  -- Note, metadata may still exist for tables that no longer exists due to orphans.
  FUNCTION ChkMBR( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(6) := 'chkMBR';  
    --
    i_count         PLS_INTEGER;
    v_sql           VARCHAR2(255);
    v_geom          VARCHAR2(30);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);
    v_table         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_tablename);
    v_schema    :=SplitOwnerObject( v_ownertable, 'OWNER');
    v_table     :=SplitOwnerObject( v_ownertable, 'TABLE');
    -- Check for geometry table existence
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    -- Run different checks depending on who you are.
    IF v_schema = USER THEN
      v_sql:='SELECT COUNT(1) FROM USER_SDO_GEOM_METADATA WHERE TABLE_NAME = :vtable AND COLUMN_NAME= :vgeom';
      EXECUTE IMMEDIATE v_sql INTO i_count USING v_table, v_geom;
    ELSE
      IF chkInsertOnMDSYS THEN
        v_sql:='SELECT COUNT(1) FROM MDSYS.SDO_GEOM_METADATA_TABLE WHERE SDO_OWNER= :vowner AND SDO_TABLE_NAME = :vtable AND SDO_COLUMN_NAME= :vgeom';
        EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_table, v_geom;
      ELSE
        v_sql:='SELECT COUNT(1) FROM ALL_SDO_GEOM_METADATA WHERE OWNER= :vowner AND TABLE_NAME = :vtable AND COLUMN_NAME= :vgeom';
        EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_table, v_geom;  
      END IF;
    END IF;
    IF i_count >= 1 THEN
      RETURN TRUE; -- Metadata exists
    ELSE
      RETURN FALSE; -- Metadata does not exist or is incomplete.
    END IF;
  EXCEPTION
    WHEN e_GeometryNotFound THEN
      RETURN FALSE;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END ChkMBR;
  ----------------------------------------------------------------------------------
  -- ChkInsertOnMDSYS is an internal function to detect insert privileges on 
  -- MDSYS.SDO_GEOM_METADATA_TABLE.
  -- Syntax: IF chkInsertOnMDSYS THEN
  -- If TRUE, current user can insert Oracle metadata for a table they do not own.
  FUNCTION chkInsertOnMDSYS RETURN BOOLEAN IS
  --
  PRAGMA AUTONOMOUS_TRANSACTION;
  c_cmdname             CONSTANT VARCHAR2(16):='chkInsertOnMDYS';
  v_sql                 VARCHAR2(2048);
  BEGIN
    v_sql:='INSERT INTO MDSYS.SDO_GEOM_METADATA_TABLE (SDO_OWNER, SDO_TABLE_NAME, SDO_COLUMN_NAME, SDO_DIMINFO) Values 
                 (USER, ''GOOM_JUNK'', ''GEOMETRY'', MDSYS.SDO_DIM_ARRAY(
                 MDSYS.SDO_DIM_ELEMENT(''X'',-2147483648.000000,2147483647.000000,0.000050),
                 MDSYS.SDO_DIM_ELEMENT(''Y'',-2147483648.000000,2147483647.000000,0.000050),
                 MDSYS.SDO_DIM_ELEMENT(''Z'',-2147483648.000000,2147483647.000000,0.000050)))';
  	EXECUTE IMMEDIATE v_sql;
  	ROLLBACK;        
  	RETURN TRUE;
  EXCEPTION
  	WHEN e_NoPrivilege THEN
      RETURN FALSE;
  	WHEN OTHERS THEN
  	  REPORT_ERROR( c_cmdname, v_sql, c_msgNone, SQLCODE, SQLERRM);
  	  RETURN FALSE;
  END chkInsertOnMDSYS;
  ----------------------------------------------------------------------------------
  -- IsGeographic returns BOOLEAN TRUE if the specified SRID is geographic based.
  -- Syntax: IF GOOM.isGeographic(i_srid) THEN;
  -- If i_srid is set to 0 (the default) then the results is always FALSE.
  FUNCTION isGeographic ( i_srid IN INTEGER DEFAULT 0) RETURN BOOLEAN IS
    c_cmdname   CONSTANT VARCHAR2(12) := 'isGeographic';
    i_count     PLS_INTEGER := 0;
    v_sql       VARCHAR2(255);
  BEGIN
    v_sql := 'SELECT COUNT(*) FROM MDSYS.GEODETIC_SRIDS WHERE SRID=:vsrid';
    EXECUTE IMMEDIATE v_sql INTO i_count USING i_srid;
    IF i_count > 0 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, 'SRID: ' || i_srid, v_sql, SQLCODE, SQLERRM);
      RETURN FALSE;
  END isGeographic;
  ----------------------------------------------------------------------------------
  -- IsText returns BOOLEAN TRUE if the indicated geometry is a GeoMedia text geometry.
  -- Syntax: IF GOOM.isText(v_tablename, v_geomcol) THEN
  FUNCTION isText( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2) RETURN BOOLEAN IS
    c_cmdname       CONSTANT VARCHAR2(6) := 'isText';
    --
    v_st            PLS_INTEGER;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);
    v_table         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable :=GetOwnerObject( v_tablename);
    v_schema     :=SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      :=SplitOwnerObject( v_ownertable, 'TABLE');
    --
    IF chkGeometry( v_ownertable, v_geomcol ) THEN
      v_sql:='SELECT NVL(DATA_SUBTYPE,0) FROM GDOSYS.GFIELDMAPPING WHERE OWNER=:vowner AND TABLE_NAME = :vtablename AND COLUMN_NAME= :columnname';
      EXECUTE IMMEDIATE v_sql INTO v_st USING v_schema, v_table, v_geomcol;
      IF v_st = 5 THEN
        RETURN TRUE;
      ELSE
        RETURN FALSE; -- not a spatial column
      END IF;
    ELSE
      RAISE e_GeometryNotFound;
    END IF;
  EXCEPTION
    WHEN e_geometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_ownertable);
      RETURN FALSE;
    WHEN OTHERS THEN
      RETURN FALSE;
  END isText;

  -- -----------------------------------------------------------------------------------------------------------------
  -- GET FUNCTIONS: These return a specific object name or value
  -- -----------------------------------------------------------------------------------------------------------------

  -- Returns the global database name.
  -- Syntax: Select GOOM.GetDBNAME from DUAL;
  FUNCTION GetDBNAME RETURN VARCHAR2 IS
    c_cmdname   VARCHAR2(9):='GetDBNAME';
    v_dbname    VARCHAR2(32);
  BEGIN
    SELECT SUBSTR(GLOBAL_NAME, 1, INSTR(GLOBAL_NAME, '.') - 1) INTO v_dbname FROM GLOBAL_NAME;
    IF v_dbname IS NULL THEN
      SELECT GLOBAL_NAME INTO v_dbname FROM GLOBAL_NAME;
    END IF;
    RETURN v_dbname;
    --
    EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, USER, c_msgNone, SQLCODE, SQLERRM);
    RETURN NULL;  
  END GetDBNAME;
  ---------------------------------------------------------------------------------
  -- Returns the Database Version number.
  -- Syntax: Select GOOM.GetDBVersion from DUAL;
  FUNCTION GetDBVERSION RETURN VARCHAR2 IS
   c_cmdname   VARCHAR2(12):='GetDBVERSION';
   v_version   VARCHAR2(12);
  BEGIN
    -- SELECT SUBSTR(BANNER,INSTR(BANNER,'R')+8,10) INTO v_version FROM V$VERSION WHERE ROWNUM=1;
    -- changed to support Oracle 12c and earlier versions.
    SELECT VERSION INTO v_version FROM PRODUCT_COMPONENT_VERSION WHERE UPPER(PRODUCT) LIKE 'ORACLE%' AND ROWNUM=1;
    RETURN v_version;
    --
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, USER, c_msgNone, SQLCODE, SQLERRM);
    RETURN NULL;  
  END GETDBVERSION;
  -- ---------------------------------------------------------------------------------
  -- GetCount returns the row count for a table as an INTEGER value.
  -- Syntax: i_count:=GOOM.GetCount(v_tablename);
  FUNCTION GetCount( v_tablename IN VARCHAR2) RETURN INTEGER IS
    c_cmdname       CONSTANT VARCHAR2(8) := 'GetCount';
    --
    v_sql           VARCHAR2(200);
    i_count         INTEGER:=0;
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject(v_tablename);
    --
    IF NOT ChkTable( v_ownertable ) AND NOT ChkView( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    v_sql := 'SELECT COUNT(1) FROM ' || v_ownertable;
    EXECUTE IMMEDIATE v_sql INTO i_count;
    RETURN i_count; -- Number of rows in the layer
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_ownertable);
      RETURN NULL;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, v_sql, SQLCODE, SQLERRM);
      RETURN NULL;
  END GetCount;
  ----------------------------------------------------------------------------------
  -- GetTableSize returns the segment size of a table in MB.
  -- Syntax: n_tabsize:=GOOM.GetTableSize(v_tablename);
  --  v_tablename : the table to process
  --
  FUNCTION GetTableSize( v_tablename IN VARCHAR2) RETURN NUMBER IS
    c_cmdname  CONSTANT VARCHAR2(12) := 'GetTableSize';
    v_sql      VARCHAR2(100);
    n_size     NUMBER(10,2);
    -- added for owner.table support
    v_ownertable  VARCHAR2(61);
    v_owner       VARCHAR2(30);
    v_table       VARCHAR2(30);
  BEGIN
    v_ownertable  := GetOwnerObject(v_tablename);
    v_owner       := SplitOwnerObject(v_ownertable,'OWNER');
    v_table       := SplitOwnerObject(v_ownertable,'TABLE');
    IF v_owner = USER THEN
      v_sql:='SELECT SUM(BYTES)/1024000 FROM USER_EXTENTS WHERE SEGMENT_NAME=:vtab';
      EXECUTE IMMEDIATE v_sql INTO n_size USING v_table;
    ELSE
      v_sql:='SELECT SUM(BYTES)/1024000 FROM DBA_EXTENTS WHERE OWNER = :vowner AND SEGMENT_NAME = :vtab';
      EXECUTE IMMEDIATE v_sql INTO n_size USING v_owner, v_table;
    END IF;
    RETURN n_size;
    EXCEPTION
      WHEN OTHERS THEN
        REPORT_ERROR( c_cmdname, v_ownertable, v_sql, SQLCODE, SQLERRM);
        RETURN 0;
  END GetTableSize;
  ----------------------------------------------------------------------------------
  -- GetDIM returns the dimension of a geometry - 2 or 3 as integer.  A null geometry
  -- will cause the GOOM package default dimension to be returned.  
  -- Syntax: v_dim:=GOOM.GetDim(v_tablename,v_geometry); 
  FUNCTION GetDim( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2) RETURN INTEGER IS
    c_cmdname       CONSTANT VARCHAR2(6) := 'GetDIM';
    --
    v_ownertable    VARCHAR2(61); 
    v_sql           VARCHAR2(512);
    v_DIM           VARCHAR2(4);
    i_cur           PLS_INTEGER;
    i_ret           PLS_INTEGER;
  BEGIN
    v_ownertable:=GetOwnerObject( v_tablename );
    v_sql := 'SELECT UNIQUE(SUBSTR(A.' || v_geomcol || '.SDO_GTYPE,1,1)) FROM ' || v_ownertable || ' A WHERE A.' || v_geomcol || '.SDO_GTYPE IS NOT NULL AND ROWNUM < '|| c_sample;
    --
    i_cur := SYS.DBMS_SQL.OPEN_CURSOR;
    SYS.DBMS_SQL.PARSE( i_cur, v_sql, SYS.DBMS_SQL.NATIVE);
    SYS.DBMS_SQL.DEFINE_COLUMN( i_cur, 1, v_DIM, 1);
    i_ret := SYS.DBMS_SQL.EXECUTE( i_cur);
    i_ret := SYS.DBMS_SQL.FETCH_ROWS( i_cur);
    SYS.DBMS_SQL.COLUMN_VALUE( i_cur, 1, v_DIM);
    SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    --
    IF v_DIM IS NULL OR v_DIM = 0 THEN
      v_DIM := c_defDim;
    END IF;
    RETURN v_DIM;
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geomcol, v_sql, SQLCODE, SQLERRM);
      RETURN c_defDim;
  END GetDim;
  ----------------------------------------------------------------------------------
  -- GetGDOTableName returns the GDOSYS tablename for the given table type.
  -- Syntax: v_gdotable:=GetGDOTableName(c_tabletype);
  -- Valid Types are:
  --  'INGRFieldLookup','GOracleFieldMapping','INGRAttributeProperties'
  --  'INGRGeometryProperties','INGRFeatures','INGRPickLists'
  --  'GCoordSystemTable','GOracleIndexColumns','GParameters','LibraryTables';
  FUNCTION GetGDOTableName( v_tabletype IN VARCHAR2) RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(16) := 'GetGDOTableName';
    c_feedback      CONSTANT VARCHAR2(32) := 'Metadata table does not exist: ';
    c_metatab       CONSTANT VARCHAR2(18) := 'GDOSYS.GALIASTABLE';
    --
    v_gdotable      VARCHAR2(64);
    v_sql           VARCHAR2(255);
    i_count         PLS_INTEGER;
  BEGIN
    IF NOT chkTable( c_metatab ) THEN
      RAISE e_TableNotFound;
    END IF;
    v_sql := 'SELECT COUNT(*) FROM GDOSYS.GALIASTABLE WHERE UPPER(TABLETYPE)=UPPER(:tabtype)';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_tabletype;
    IF i_count <> 0 THEN
      v_sql := 'SELECT TABLENAME FROM GDOSYS.GALIASTABLE WHERE UPPER(TABLETYPE)=UPPER(:tabtype)';
      EXECUTE IMMEDIATE v_sql INTO v_gdotable USING v_tabletype;
      RETURN v_gdotable;
    ELSE
      Response( c_cmdname || c_msgWarning, c_feedback || v_tabletype);
      RETURN NULL;
    END IF;
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgWarning, c_msgTableNotFound || c_metatab);
      RETURN NULL;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_tabletype, v_sql, SQLCODE, SQLERRM);
      RETURN NULL;
  END GetGDOTableName;
  ----------------------------------------------------------------------------------
  -- GetGEOM returns the first geometry column found in a table as VARCHAR2(30)
  -- Syntax: v_geometry:=GetGeom(v_tablename);
  FUNCTION GetGeom( v_tablename IN VARCHAR2 ) RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(7) := 'GetGEOM';
    -- 
    v_sql           VARCHAR2(255);
    v_geometry      VARCHAR2(30);
    i_cur           PLS_INTEGER;
    i_ret           PLS_INTEGER;
    i_count         PLS_INTEGER;
    -- Added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);    
    v_table         VARCHAR2(30); 
  BEGIN
    -- Added for owner.table support
    v_ownertable := GetOwnerObject( v_tablename );
    v_schema     := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      := SplitOwnerObject( v_ownertable, 'TABLE');
    --
    v_sql := 'SELECT COUNT(*) FROM ALL_TAB_COLUMNS WHERE DATA_TYPE=''SDO_GEOMETRY'' AND OWNER=:vowner AND TABLE_NAME=:vtable';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_table;
    v_sql := 'SELECT COLUMN_NAME FROM ALL_TAB_COLUMNS WHERE DATA_TYPE=''SDO_GEOMETRY'' AND TABLE_NAME=''' || v_table || ''' AND OWNER='''||v_schema||''' ORDER BY COLUMN_NAME';
    --
    i_cur := SYS.DBMS_SQL.OPEN_CURSOR;
    SYS.DBMS_SQL.PARSE( i_cur, v_sql, SYS.DBMS_SQL.NATIVE);
	SYS.DBMS_SQL.DEFINE_COLUMN( i_cur, 1, v_geometry, 30);
	i_ret := SYS.DBMS_SQL.EXECUTE( i_cur);
	i_ret := SYS.DBMS_SQL.FETCH_ROWS( i_cur);
	SYS.DBMS_SQL.COLUMN_VALUE( i_cur, 1, v_geometry);
	SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    IF i_count > 1 THEN
	  Response( c_cmdname || c_msgWarning,'Multiple ('|| i_count ||') geometries found in: '|| v_ownertable ||', first geometry returned.');
    END IF;
    RETURN v_geometry; --Return the name of the first geometry column found or NULL.
  EXCEPTION
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      REPORT_ERROR( c_cmdname, v_tablename, v_sql, SQLCODE, SQLERRM);
      RETURN NULL;
  END GetGeom;
  ----------------------------------------------------------------------------------
  -- GetGTYPE returns the GTYPE as VARCHAR2(4) for the input table and geometry column name.
  -- Syntax: v_GTYPE:=GetGTYPE(v_tablename,v_geometry);
  FUNCTION GetGTYPE( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(8) := 'GetGTYPE';
    --
    v_sql           VARCHAR2(255);
    v_gtype         VARCHAR2(4);
    i_cur           PLS_INTEGER;
    i_dim           PLS_INTEGER;
    i_ret           PLS_INTEGER;
    i_count         PLS_INTEGER;
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_geom          VARCHAR2(61);
  BEGIN
    IF v_tablename is NULL THEN
      RAISE e_CannotBeNull;
    END IF;
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_tablename);
    --
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable);
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    v_sql := 'SELECT COUNT(UNIQUE(A.' || v_geom || '.SDO_GTYPE)) FROM ' || v_ownertable || ' A WHERE A.'|| v_geom ||' IS NOT NULL';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    i_dim := GetDIM( v_ownertable, v_geom);
    IF i_count > 1 AND i_dim < 3 THEN
      v_gtype := '2004';      -- More than one kind of GTYPE so it is Compound 2D.
    ELSIF i_count > 1 AND i_dim = 3 THEN
      v_gtype := '3004';      -- More than one kind of GTYPE so it is Compound 3D.
    ELSE                      -- Only one so get the GTYPE from first row.
      v_SQL := 'SELECT UNIQUE(A.' || v_geom || '.SDO_GTYPE) FROM ' || v_ownertable|| ' A WHERE A.'|| v_geom ||' IS NOT NULL AND ROWNUM <'||c_sample;
      --
      i_cur := SYS.DBMS_SQL.OPEN_CURSOR;
      SYS.DBMS_SQL.PARSE( i_cur, v_sql, SYS.DBMS_SQL.NATIVE);
      SYS.DBMS_SQL.DEFINE_COLUMN( i_cur, 1, v_gtype, 4);
      i_ret := SYS.DBMS_SQL.EXECUTE( i_cur);
      i_ret := SYS.DBMS_SQL.FETCH_ROWS( i_cur);
      SYS.DBMS_SQL.COLUMN_VALUE( i_cur, 1, v_gtype);
      SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
    END IF;
    RETURN v_gtype; -- The GTYPE of the layer, handle null values.
  EXCEPTION
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_tablename ||'.'|| v_geom);
      RETURN NULL;
    WHEN e_CannotBeNull THEN
      Response( c_cmdname || c_msgError, c_msgCannotBeNull || v_tablename ||'.'|| v_geom);
      RETURN NULL;
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geomcol, v_sql, SQLCODE, SQLERRM);
      RETURN NULL;
  END GetGTYPE;
  ----------------------------------------------------------------------------------
  -- GetGeomType returns the GDO Geometry Type as VARCHAR2 for the input table and geometry column name.
  -- Syntax: v_geomtype:=GetGeomType(c_tablename,c_geometry);  
  FUNCTION GetGeomType( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) RETURN VARCHAR2 IS
    c_cmdname    CONSTANT VARCHAR2(11) := 'GetGeomType';
    --
    v_gtype      VARCHAR2(4);
    v_geomtype   VARCHAR2(16);
    v_geom       VARCHAR2(30);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_tablename);
    --
    IF v_geomcol IS NULL THEN
      v_geom := GetGEOM( v_ownertable);
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    v_gtype:=SUBSTR(GetGTYPE( v_ownertable, v_geom),4);
    CASE v_gtype
      WHEN 1 THEN v_geomtype:='MULTIPOINT';     -- POINT:   Changed to multipoint for better GM compatibility
      WHEN 2 THEN v_geomtype:='MULTILINE';      -- LINE:    Changed to multiline for better GM compatibility
      WHEN 3 THEN v_geomtype:='MULTIPOLYGON';   -- POLYGON: Changed to multipolygon for better GM compatibility
      WHEN 4 THEN v_geomtype:='COLLECTION';
      WHEN 5 THEN v_geomtype:='MULTIPOINT';
      WHEN 6 THEN v_geomtype:='MULTILINE';
      WHEN 7 THEN v_geomtype:='MULTIPOLYGON';
      ELSE v_geomtype:='UNKNOWN';
    END CASE;
    RETURN v_geomtype;
  EXCEPTION
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_tablename ||'.'|| v_geom);
      RETURN 'UNKNOWN';
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geom, v_gtype, SQLCODE, SQLERRM);
      RETURN 'UNKNOWN';
  END GetGeomType;
  ----------------------------------------------------------------------------------
  -- GetINDXNAME returns a unique index name based on the tablename.
  -- Syntax: v_index:=getINDXNAME(v_tablename,[v_extend]);
  --         v_extend : the optional override extension to use.  For example, _SIDX.
  FUNCTION GetINDXNAME( v_tablename IN VARCHAR2, v_extend IN VARCHAR2 DEFAULT NULL) RETURN VARCHAR2 IS
    c_cmdname   CONSTANT VARCHAR2(11) := 'getINDXNAME';
    v_iname     VARCHAR2(61);
    v_icount    PLS_INTEGER := 1;
    v_ipad      VARCHAR2(2);
    v_len       PLS_INTEGER := 22;
    v_ext       VARCHAR2(3);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);
    v_table         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_tablename);
    v_schema    :=SplitOwnerObject( v_ownertable,'OWNER');
    v_table     :=SplitOwnerObject( v_ownertable,'TABLE'); 
    -- Create an index as  of table name and add _SI extension
    IF v_extend IS NULL THEN
      v_ext := c_idxext;
    ELSE
      v_ext := v_extend;
    END IF;
    v_iname := v_schema||'.'||SUBSTR( v_table, 1, v_len) || v_ext;
    WHILE chkIndex(v_iname) = TRUE -- If index exists...
    LOOP
      -- Loop until a unique name is created.
      v_ipad   := TO_CHAR( v_icount); -- Convert counter to character.
      v_iname  := v_schema||'.'||SUBSTR( v_table, 1, v_len) || v_ext || v_ipad; -- Create a new index name.
      v_icount := v_icount + 1; -- Increment counter.
    END LOOP;
    RETURN v_iname; -- Return unique index name.
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, 'INDEX: ' || v_iname, SQLCODE, SQLERRM);
  END GetINDXNAME;
  ----------------------------------------------------------------------------------
  -- GetKeyCol returns the primary key column of a table containing a single key.
  -- Syntax: v_keycol:=GetKeyCol(v_tablename);
  FUNCTION GetKeyCol( v_tablename IN VARCHAR2) RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(14) := 'GetKeyCol';
    c_feedback      CONSTANT VARCHAR2(40) := 'No single key column found, key count: ';
    --
    v_sql           VARCHAR2(512);
    v_pkey          VARCHAR2(32);
    i_count         PLS_INTEGER;
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);    
    v_table         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_tablename);
    v_schema    :=SplitOwnerObject( v_ownertable,'OWNER');
    v_table     :=SplitOwnerObject( v_ownertable,'TABLE'); 
    --
    v_sql:='SELECT COUNT(*) FROM ALL_CONS_COLUMNS WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND CONSTRAINT_NAME IN 
           (SELECT CONSTRAINT_NAME FROM ALL_CONSTRAINTS WHERE CONSTRAINT_TYPE=''P'' AND OWNER=:vowner AND TABLE_NAME=:vtable)';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_table, v_schema, v_table;
    IF i_count=0 OR i_count>1 THEN
      Response( c_cmdname || c_msgWarning, c_feedback || i_count);
      RETURN 'NO_KEY';
    ELSE
      v_sql:='SELECT COLUMN_NAME FROM ALL_CONS_COLUMNS WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND CONSTRAINT_NAME IN 
             (SELECT CONSTRAINT_NAME FROM ALL_CONSTRAINTS WHERE CONSTRAINT_TYPE=''P'' AND OWNER=:vowner AND TABLE_NAME=:vtable)';
      EXECUTE IMMEDIATE v_sql INTO v_pkey USING v_schema, v_table, v_schema, v_table;
      RETURN v_pkey; --Return the primary key of the table
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, v_table, v_sql, SQLERRM);
  END GetKeyCol;
  ----------------------------------------------------------------------------------
  -- GetViewKeyCol returns the primary key column assigned to a view based on metadata
  -- information in GDOSYS.GINDEX_COLUMNS.
  -- Syntax: v_keycol:=GetViewKeyCol(v_viewname);
  FUNCTION GetViewKeyCol( v_viewname IN VARCHAR2) RETURN VARCHAR2 IS
    c_cmdname       CONSTANT VARCHAR2(14) := 'GetViewKeyCol';
    c_feedback      CONSTANT VARCHAR2(40) := 'No single key column found, key count: ';
    --
    v_sql           VARCHAR2(512);
    v_pkey          VARCHAR2(32);
    i_count         PLS_INTEGER;
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);    
    v_view         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_viewname);
    v_schema    :=SplitOwnerObject( v_ownertable,'OWNER');
    v_view      :=SplitOwnerObject( v_ownertable,'TABLE'); 
    --
    BEGIN
      v_sql:='SELECT COUNT(*) FROM GDOSYS.GINDEX_COLUMNS WHERE OWNER=:vowner AND OBJECT_NAME=:vview';
      EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_view;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN 
      i_count:=0;
    END;
    IF i_count=0 OR i_count>1 THEN
      Response( c_cmdname || c_msgWarning, c_feedback || i_count);
      RETURN 'NO_KEY';
    ELSE
      v_sql:='SELECT COLUMN_NAME FROM GDOSYS.GINDEX_COLUMNS WHERE OWNER=:vowner AND OBJECT_NAME=:vview';
      EXECUTE IMMEDIATE v_sql INTO v_pkey USING v_schema, v_view;
      RETURN v_pkey; --Return the primary key of the view
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, v_view, v_sql, SQLERRM);
  END GetViewKeyCol;
  ----------------------------------------------------------------------------------
  -- GetColumnType returns the data type for the specified primary key column.
  -- Syntax: v_coltype:=GetColumnType(v_tablename,v_column);
  FUNCTION GetColumnType ( v_tablename IN VARCHAR2, v_column IN VARCHAR2) RETURN VARCHAR2 IS
    c_cmdname           CONSTANT VARCHAR2(32):='GetColumnType';
    --
    v_sql               VARCHAR2(1024);
    v_coltype           VARCHAR2(30);
    v_datatype          VARCHAR2(30);
    v_length            INTEGER;
    v_precision         INTEGER;
    v_scale             INTEGER;
    -- added for owner.table support
    v_ownertable        VARCHAR2(61);
    v_schema            VARCHAR2(30);    
    v_table             VARCHAR2(30);
  --
  BEGIN
   -- added for owner.table support
   v_ownertable :=GetOwnerObject( v_tablename);
   v_schema     :=SplitOwnerObject( v_ownertable,'OWNER');
   v_table      :=SplitOwnerObject( v_ownertable,'TABLE');
   -- Verify inputs
   IF NOT chkColumn( v_ownertable, v_column ) THEN
     RAISE e_ColumnNotFound;
   END IF;
   --
   v_sql:='SELECT DATA_TYPE FROM ALL_TAB_COLUMNS WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND COLUMN_NAME=:vkeycol';
   EXECUTE IMMEDIATE v_sql INTO v_datatype USING v_schema, v_table, v_column;
   CASE v_datatype
   WHEN 'VARCHAR2' THEN
     v_sql:='SELECT DATA_LENGTH FROM ALL_TAB_COLUMNS WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND COLUMN_NAME=:vkeycol';
     EXECUTE IMMEDIATE v_sql INTO v_length USING v_schema, v_table, v_column;
     v_coltype := 'VARCHAR2('|| v_length ||')';
   WHEN 'NUMBER' THEN
     v_sql:='SELECT DATA_PRECISION FROM ALL_TAB_COLUMNS WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND COLUMN_NAME=:vkeycol';
     EXECUTE IMMEDIATE v_sql INTO v_precision USING v_schema, v_table, v_column;
     v_sql:='SELECT DATA_SCALE FROM ALL_TAB_COLUMNS WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND COLUMN_NAME=:vkeycol';
     EXECUTE IMMEDIATE v_sql INTO v_scale USING v_schema, v_table, v_column;
     IF v_precision IS NULL AND v_scale = 0 THEN
        v_coltype:='INTEGER';
     ELSIF v_precision IS NOT NULL AND v_scale = 0 THEN
        v_coltype:='NUMBER('||TO_CHAR( v_precision )||')';
     ELSE
        v_coltype:='NUMBER('||TO_CHAR( v_precision )||','||TO_CHAR( v_scale )||')';
     END IF;
   ELSE
     v_coltype := v_datatype;
   END CASE;
   RETURN v_coltype;
  EXCEPTION
     WHEN e_ColumnNotFound THEN
      Response( c_cmdname || c_msgError, c_msgColumnNotFound || v_column);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_column, c_msgGoomInternal);
     WHEN OTHERS THEN
        REPORT_ERROR( c_cmdname, v_ownertable ||'.'|| v_column, v_sql, v_datatype ||':'|| v_coltype,sqlerrm);
  END GetColumnType;
  ----------------------------------------------------------------------------------
  -- GetOwnerObject returns the table or view in the format OWNER.OBJECT (VARCHAR2(61))
  -- Syntax: v_ownertable:=GOOM.GetOwnerObject(c_tablename);
  --         The owner portion comes from the USER calling the function.
  FUNCTION GetOwnerObject( v_tablename IN VARCHAR2 ) RETURN VARCHAR2 IS
    c_cmdname   CONSTANT VARCHAR2(30):='GetOwnerObject';
    v_schema    VARCHAR2(30) := NULL;
  BEGIN
    v_schema := SUBSTR(UPPER( v_tablename ),1,INSTR(UPPER( v_tablename ),'.')-1);
    IF v_schema IS NULL THEN
       v_schema:=USER;
       RETURN UPPER( v_schema ||'.'||UPPER( v_tablename ));
    ELSE
       RETURN UPPER( v_tablename );
    END IF;
  EXCEPTION
     WHEN OTHERS THEN
        REPORT_ERROR ( c_cmdname, v_schema, v_tablename, SQLCODE, SQLERRM);
  END GetOwnerObject;
  ----------------------------------------------------------------------------------
  -- SplitOwnerObject returns either the Owner name or the Object name from the owner.object format.
  -- Syntax:  v_schema:=GOOM.SplitOwnerObject(v_tablename,v_type);
  --          v_schema:=GOOM.SplitOwnerObject(v_tablename,'OWNER');
  --          v_table:=GOOM.SplitOwnerObject(v_tablename,'OBJECT');
  --          Valid c_types are OWNER/USER or TABLE/OBJECT (use TABLE for views).
  --
  FUNCTION SplitOwnerObject( v_tablename IN VARCHAR2, v_type IN VARCHAR2) RETURN VARCHAR2 IS
    c_cmdname   CONSTANT VARCHAR2(30) := 'SplitOwnerObject';
    c_feedback  CONSTANT VARCHAR2(70) := 'Invalid Type Specified (OWNER/USER or TABLE/OBJECT)';
    --
    v_schema    VARCHAR2(30);
    v_table     VARCHAR2(30);
  BEGIN
    v_schema:=SUBSTR(UPPER( v_tablename ),1,INSTR(UPPER( v_tablename ),'.')-1);
    IF v_schema IS NULL THEN
       v_schema := USER;
       v_table  := UPPER( v_tablename );
    ELSE
       v_table := SUBSTR(UPPER( v_tablename ),INSTR(UPPER( v_tablename ),'.',1)+1);
    END IF;
    IF UPPER( v_type ) = 'TABLE' OR UPPER( v_type ) = 'OBJECT' OR UPPER( v_type ) = 'VIEW' THEN
      RETURN v_table;
    ELSIF UPPER( v_type ) = 'OWNER' OR UPPER( v_type ) = 'USER' OR UPPER( v_type ) = 'SCHEMA'  THEN
      RETURN v_schema;
    ELSE
      RESPONSE( c_cmdname || c_msgWarning, c_feedback);
      RETURN NULL;
    END IF;
  EXCEPTION
     WHEN OTHERS THEN
        REPORT_ERROR ( c_cmdname, v_tablename, v_schema, v_table,sqlerrm);
  END SplitOwnerObject;
  ----------------------------------------------------------------------------------------
  -- GetSRID returns the first SRID found in the geometry column or it returns NULL.
  -- Syntax: i_srid:=GetSRID(v_tablename, v_geomcol);
  --
  FUNCTION GetSRID( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) RETURN INTEGER IS
    c_cmdname           CONSTANT VARCHAR2(7)  := 'GetSRID';
    c_feedback          CONSTANT VARCHAR2(30) := 'Multiple SRIDs found in: ';
    c_feedback2         CONSTANT VARCHAR2(30) := ', only one SRID is allowed.';
    --
    v_geom              VARCHAR2(30);
    i_srid              PLS_INTEGER:=0;
    v_sql               VARCHAR2(255);
    v_ownertable        VARCHAR2(61);
    i_cur               PLS_INTEGER;
    i_ret               PLS_INTEGER;
    i_count             PLS_INTEGER;
    v_schema            VARCHAR2(30);    
    v_table             VARCHAR2(30);
  BEGIN
    v_ownertable :=GetOwnerObject( v_tablename);
    v_schema     :=SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      :=SplitOwnerObject( v_ownertable, 'TABLE');
    -- Input checks
    IF NOT ChkTable( v_ownertable ) AND NOT ChkView ( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF NOT ChkGeometry( v_ownertable, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    -- Look for SRID first in table but if table is empty, look at Oracle metadata.
    IF chkTableData(v_ownertable) THEN
      v_sql := 'SELECT COUNT(UNIQUE(NVL(A.'|| v_geom ||'.SDO_SRID,0))) FROM '|| v_ownertable ||' A WHERE A.'|| v_geom ||' IS NOT NULL';
      EXECUTE IMMEDIATE v_sql INTO i_count;
      v_sql := 'SELECT UNIQUE(NVL(A.'|| v_geom ||'.SDO_SRID,0)) FROM '|| v_ownertable ||' A WHERE A.'|| v_geom ||' IS NOT NULL AND ROWNUM < '||c_sample;
      --
      i_cur := SYS.DBMS_SQL.OPEN_CURSOR;
      SYS.DBMS_SQL.PARSE( i_cur, v_sql, SYS.DBMS_SQL.NATIVE);
	  SYS.DBMS_SQL.DEFINE_COLUMN( i_cur, 1, i_srid);
	  i_ret := SYS.DBMS_SQL.EXECUTE( i_cur);
	  i_ret := SYS.DBMS_SQL.FETCH_ROWS( i_cur);
	  SYS.DBMS_SQL.COLUMN_VALUE( i_cur, 1, i_srid);
	  SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      IF i_count > 1 THEN
	    Response( c_cmdname || c_msgWarning, c_feedback || v_ownertable ||'.'|| v_geom || c_feedback2 );
      END IF;
    ELSIF chkMBR(v_ownertable,v_geom) THEN
      v_sql := 'SELECT SRID FROM ALL_SDO_GEOM_METADATA WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND COLUMN_NAME=:vgeomcol';
      EXECUTE IMMEDIATE v_sql INTO i_srid USING v_schema, v_table, v_geom;
    ELSE
      i_srid := 0;
    END IF;
    -- Return result
    IF i_srid = 0 THEN
      RETURN NULL;
    ELSE
      RETURN i_srid;
    END IF;
  EXCEPTION
    WHEN e_NoDataFound THEN
      Response( c_cmdname || c_msgerror, c_msgNoDataFound || v_ownertable);
      RETURN NULL;
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgTableNotFound || v_ownertable);
      RETURN NULL;
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgGeometryNotFound || v_geomcol);
      RETURN NULL;    
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      REPORT_ERROR( c_cmdname, v_ownertable ||'.'|| v_geom, v_sql, SQLCODE, SQLERRM);
      RETURN NULL;
  END GetSRID;
  ----------------------------------------------------------------------------------------
  -- GetCSName returns coordinate system name for the specified SRID.
  -- Syntax: v_CSName := GDORASTER.GetCSName(i_srid);
  --  i_srid : the srid you need the name for.
  --
  FUNCTION GetCSName( i_srid IN INTEGER) RETURN VARCHAR2 IS
  c_cmdname  CONSTANT VARCHAR2(9) := 'GetCSName';
  v_sql      VARCHAR2(100);
  v_csname   VARCHAR2(80);
  BEGIN
    v_sql:='SELECT NVL(CS_NAME,''Invalid SRID'') FROM MDSYS.CS_SRS WHERE SRID=:srid';
    EXECUTE IMMEDIATE v_sql INTO v_csname USING i_srid;
    RETURN v_csname;
    EXCEPTION
      WHEN OTHERS THEN
        RETURN 'Invalid SRID';
  END GetCSName;
  --------------------------------------------------------------------------------------
  -- GetPrimaryGeom returns the primary geometry field name used by GeoMedia.  This is only
  -- necessary when more than one geometry is present in a feature class.
  -- Syntax: v_primegeom:=GOOM.GetPrimaryGeom(v_tablename);
  --
  FUNCTION GetPrimaryGeom( v_tablename IN VARCHAR2) RETURN VARCHAR2 IS
    c_cmdname           CONSTANT VARCHAR2(15) := 'GetPrimaryGeom';
    --
    v_ownertable        VARCHAR2(61);
    v_primarygeom       VARCHAR2(30);
    v_sql               VARCHAR2(155);
  BEGIN
    v_ownertable := GetOwnerObject( v_tablename );
    --
    v_sql:='SELECT PRIMARYGEOMETRYFIELDNAME FROM GDOSYS.GFEATURES WHERE FEATURENAME=:ownertable';
    EXECUTE IMMEDIATE v_sql INTO v_primarygeom USING v_ownertable;
    RETURN v_primarygeom;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, v_ownertable, v_sql, sqlcode, sqlerrm);
      RETURN NULL;
  END GetPrimaryGeom;

  -- -----------------------------------------------------------------------------------------------------------------
  -- ORACLE METADATA PROCEDURES: Operate on USER_SDO_GEOM_METADATA or ALL_SDO_GEOM_METADATA
  -- -----------------------------------------------------------------------------------------------------------------
  
  -- InsertMBR is an internal procedure used to insert MBR values into USER_SDO_GEOM_METADATA.
  -- EXEC InsertMBR(i_dim,v_tablename,v_geometry,n_xmin,n_xmax,n_ymin,n_ymax,[n_tol],[i_srid]);
  --  i_dim = 2 for 2D, all other i_dim values result in 3D.
  --  [n_tol] : Optional tolerance value Default is 0.00005
  --  [i_srid}: Optional SRID Default is NULL
  --
  PROCEDURE InsertMBR( i_dim       IN INTEGER,
                       v_tablename IN VARCHAR2,
                       v_geomcol   IN VARCHAR2 DEFAULT NULL,
                       n_xmin      IN NUMBER   DEFAULT -2147483648,
                       n_xmax      IN NUMBER   DEFAULT  2147483648,
                       n_ymin      IN NUMBER   DEFAULT -2147483648,
                       n_ymax      IN NUMBER   DEFAULT  2147483648,
                       n_tol       IN NUMBER   DEFAULT  0.00005,
                       i_srid      IN INTEGER  DEFAULT NULL) AS
    --
    c_cmdname       CONSTANT VARCHAR2(9) := 'InsertMBR';
    --
    v_debug         VARCHAR2(255);
    v_sql           VARCHAR2(1024);
    v_geom          VARCHAR2(30);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    v_table         VARCHAR2(30);
    --
  BEGIN
    -- added for owner.table support
    v_ownertable := GetOwnerObject( v_tablename);
    v_schema     := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      := SplitOwnerObject( v_ownertable, 'TABLE'); 
    --
    IF NOT ChkTable( v_ownertable ) AND NOT chkView( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF NOT ChkGeometry( v_ownertable, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    --
    IF v_schema = USER THEN
      v_sql:= 'DELETE FROM USER_SDO_GEOM_METADATA WHERE TABLE_NAME = :vtable AND COLUMN_NAME = :vgeometry';
      EXECUTE IMMEDIATE v_sql USING v_table, v_geom;
      COMMIT;
      IF i_dim = 2 THEN
        v_debug := 'Inserting 2D Extents to USER_SDO_GEOM_METADATA';
        v_sql   :='INSERT INTO USER_SDO_GEOM_METADATA VALUES (:vtables,:vgeometry,
                   MDSYS.SDO_DIM_ARRAY(MDSYS.SDO_DIM_ELEMENT(''X'',:vxmin,:vxmax,:vtol),MDSYS.SDO_DIM_ELEMENT(''Y'',:vymin,:vymax,:vtol)),:vsrid)';
        EXECUTE IMMEDIATE v_sql USING v_table, v_geom, n_xmin, n_xmax, n_tol, n_ymin, n_ymax, n_tol, i_srid;
        COMMIT;      
      ELSIF i_dim = 3 THEN
        v_debug := 'Inserting 3D Extents to USER_SDO_GEOM_METADATA';
        v_sql   :='INSERT INTO USER_SDO_GEOM_METADATA VALUES(:vtables,:vgeometry,
                   MDSYS.SDO_DIM_ARRAY(MDSYS.SDO_DIM_ELEMENT(''X'',:vxmin,:vxmax,:vtol),MDSYS.SDO_DIM_ELEMENT(''Y'',:vymin,:vymax,:vtol),
                   MDSYS.SDO_DIM_ELEMENT(''Z'',:vzmin,:vzmax,:vtol)),:vsrid)';
        EXECUTE IMMEDIATE v_sql USING v_table, v_geom, n_xmin, n_xmax, n_tol, n_ymin, n_ymax, n_tol, c_ZLO, c_ZHI, n_tol, i_srid;
        COMMIT;
      ELSE 
        RAISE e_InvalidDimension;
      END IF;
    ELSIF v_schema != USER AND chkInsertOnMDSYS THEN
      v_sql:= 'DELETE FROM MDSYS.SDO_GEOM_METADATA_TABLE WHERE SDO_OWNER = :vowner
               AND SDO_TABLE_NAME = :vtable AND SDO_COLUMN_NAME = :vgeometry';
      EXECUTE IMMEDIATE v_sql USING v_schema, v_table, v_geom;
      COMMIT;
      IF i_dim = 2 THEN
        v_debug := 'Inserting 2D Extents to MDSYS.SDO_GEOM_METADATA_TABLE';
        v_sql   :='INSERT INTO MDSYS.SDO_GEOM_METADATA_TABLE VALUES (:vowner,:vtables,:vgeometry,MDSYS.SDO_DIM_ARRAY
                  (MDSYS.SDO_DIM_ELEMENT(''X'',:vxmin,:vxmax,:vtol),MDSYS.SDO_DIM_ELEMENT(''Y'',:vymin,:vymax,:vtol)),:vsrid)';
        EXECUTE IMMEDIATE v_sql USING v_schema, v_table, v_geom, n_xmin, n_xmax, n_tol, n_ymin, n_ymax, n_tol, i_srid;
        COMMIT;
      ELSIF i_dim = 3 THEN
        v_debug := 'Inserting 3D Extents to MDYSY.SDO_GEOM_METADATA_TABLE';
        v_sql   :='INSERT INTO MDSYS.SDO_GEOM_METADATA_TABLE VALUES (:vowner,:vtables,:vgeometry,MDSYS.SDO_DIM_ARRAY
                  (MDSYS.SDO_DIM_ELEMENT(''X'',:vxmin,:vxmax,:vtol),MDSYS.SDO_DIM_ELEMENT(''Y'',:vymin,:vymax,:vtol),
                   MDSYS.SDO_DIM_ELEMENT(''Z'',:vzmin,:vzmax,:vtol)),:vsrid)';
        EXECUTE IMMEDIATE v_sql USING v_schema, v_table, v_geom, n_xmin, n_xmax, n_tol, n_ymin, n_ymax, n_tol, c_ZLO, c_ZHI, n_tol, i_srid;
        COMMIT;
      ELSE
        RAISE e_InvalidDimension;
      END IF;
    ELSE
      RAISE e_NoMetadataAccess;
    END IF;
  EXCEPTION
    WHEN e_InvalidDimension THEN
      Response( c_cmdname || c_msgError, c_msgInvalidDimension || i_dim);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgInvalidDimension || i_dim, c_msgGoomInternal);      
    WHEN e_NoMetadataAccess THEN
      Response( c_cmdname || c_msgError, c_msgNoMetadataAccess || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgNoMetadataAccess || v_ownertable, c_msgGoomInternal);
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_ownertable, c_msgGoomInternal);
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_ownertable ||'.'|| v_geom);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgGeometryNotFound || v_ownertable ||'.'|| v_geom, c_msgGoomInternal);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geomcol, v_debug, v_sql, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geom, c_msgOraError || SQLCODE);
  END InsertMBR;
  -----------------------------------------------------------------------------------------
  -- SetMBR sets the default MBR for a table's geometry column in USER_SDO_GEOM_METADATA.
  -- Syntax: EXEC GOOM.SetMBR(v_tablename,[v_geomcol],[i_dimension]);
  --         c_geomcol : the geometry column.  If not specified, the procedure will automatically
  --                     use the first one it finds.
  --         i_dimension : the dimension to use, only specify this (2 or 3) if the geometry is empty.
  --
  PROCEDURE SetMBR( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, i_dimension IN INTEGER DEFAULT NULL) IS
    --
    c_cmdname       CONSTANT VARCHAR2(8)  := 'SetMBR';
    c_cmdtype       CONSTANT VARCHAR2(4)  := 'MBR';
    c_feedback1     CONSTANT VARCHAR2(41) := 'Default Geographic extents inserted for: ';
    c_feedback2     CONSTANT VARCHAR2(41) := 'Default Projected extents inserted for: ';
    --
    i_dim           INTEGER;
    v_debug         VARCHAR2(255);
    i_srid          INTEGER := NULL;
    v_geom          VARCHAR2(30);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    --
  BEGIN
    -- added for owner.table support
    v_ownertable := GetOwnerObject( v_tablename);
    v_schema     := SplitOwnerObject( v_ownertable, 'OWNER');
    -- Check to make sure input is table or view.
    IF NOT ChkTable( v_ownertable ) AND NOT ChkView( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    -- Get the geometry column name
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    -- Make sure geometry column exists...
    IF NOT ChkGeometry( v_ownertable, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    --  Use dimension override if set, otherwise calculate or use default.
    IF i_dimension IS NULL OR i_dimension < 2 OR i_dimension >3 THEN
      v_debug := 'Get Dimension';
      i_dim   := GetDIM( v_ownertable, v_geom);
    ELSE
      i_dim   := i_dimension;
    END IF;
    -- Get the SRID if one exists...
    v_debug := 'Get SRID';
    i_srid  := GetSRID( v_ownertable, v_geom);
    IF i_srid = 0 THEN
       i_srid := NULL;
    END IF;
    -- Check whether the SRID is geographic and write appropriate range
    v_debug:='Insert the MBR';
    IF i_srid IS NOT NULL AND isGeographic( i_srid ) THEN 
      InsertMBR( i_dim, v_ownertable, v_geom, -180, 180, -90, 90, c_gtol, i_srid);
      Response( c_cmdname, c_feedback1 || v_ownertable ||'.'|| v_geom);
      WRITE_RESULTS( v_schema, c_cmdType, c_cmdname, v_ownertable, c_feedback1 || v_ownertable ||'.'|| v_geom);
    ELSE -- data is assumed to be projected so write default projected range
      InsertMBR( i_dim, v_ownertable, v_geom, c_XLO, c_XHI, c_YLO, c_YHI, c_tol, i_srid);
      Response( c_cmdname, c_feedback2 || v_ownertable ||'.'|| v_geom);
      WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable, c_feedback2 || v_ownertable ||'.'|| v_geom);
    END IF;
  EXCEPTION
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_ownertable ||'.'|| v_geom);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgGeometryNotFound || v_ownertable ||'.'|| v_geom, c_msgGoomInternal);
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_ownertable, c_msgGoomInternal);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geom, v_debug, i_dim ||':'|| NVL( i_srid,0), SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geom, c_msgOraError || SQLCODE);
  END SetMBR;
  -----------------------------------------------------------------------------------------
  -- SetMBRAll sets the default MBR in USER_SDO_GEOM_METADATA for all geometry columns in the schema.
  -- Syntax: EXEC GOOM.SetMBRAll([v_schema]);
  --         v_schema : optional and should only be used when a DBA is running the command for the specified
  --                    user.
  --         i_dimension : the optional dimension to use, only specify this (2 or 3) if the geometry is empty.
  --
  PROCEDURE SetMBRAll( v_schema IN VARCHAR2 DEFAULT USER, i_dimension IN INTEGER DEFAULT NULL) IS
  c_cmdname       CONSTANT VARCHAR2(9) := 'SetMBRAll';
  c_cmdtype       CONSTANT VARCHAR2(4) := 'MBR';
  c_appliesto     CONSTANT VARCHAR2(19):= 'All Feature Classes';  
  -- 
  v_feature       GetGeomBasedFCs%ROWTYPE;
  v_ownertable    VARCHAR2(61);
  BEGIN
    ProcessStart( c_cmdname, v_schema);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_msgStart);
    FOR v_feature IN GetGeomBasedFCs( v_schema ) LOOP
      v_ownertable := v_schema ||'.'|| v_feature.TABLE_NAME;
      SetMBR( v_ownertable, v_feature.COLUMN_NAME, i_dimension);
    END LOOP;
    ProcessComplete( c_cmdname, v_schema);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_msgComplete || v_schema);
  EXCEPTION 
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, c_cmdtype, v_ownertable, v_feature.COLUMN_NAME, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_schema ||'.'|| v_feature.TABLE_NAME, c_msgOraError || SQLCODE);
  END SetMBRAll;
  -----------------------------------------------------------------------------------------
  -- SetMBRGeo sets USER_SDO_MEOM_METADATA to +-180, +-90 for all feature classes.
  -- Syntax: EXEC GOOM.SetMBRGeo([v_tablename],[i_dimension]);
  --         v_tablename : optional.  If set, only the table name is processed, otherwise all
  --                       tables in the schema are processed automatically. 
  --         i_dimension : the optional dimension to use, only specify this (2 or 3) if the geometry is empty.
  PROCEDURE SetMBRGeo( v_tablename IN VARCHAR2 DEFAULT 'ALL', i_dimension IN INTEGER DEFAULT NULL) IS
    c_cmdname       CONSTANT VARCHAR2(9) := 'SetMBRGeo';
    c_cmdtype       CONSTANT VARCHAR2(4) := 'MBR';
    c_appliesto     CONSTANT VARCHAR2(21):= 'All Feature Classes';
    c_feedback1     CONSTANT VARCHAR2(42):= 'Inserted default geographic extents for: ';
    --
    v_feature       GetGeomBasedFCs%ROWTYPE;
    v_geometry      COLS.COLUMN_NAME%TYPE;
    v_geom          COLS.COLUMN_NAME%TYPE;
    i_dim           INTEGER;
    i_srid          INTEGER := NULL;
    v_debug         VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    v_table         VARCHAR2(30);
  BEGIN
    IF v_tablename = 'ALL' THEN
      ProcessStart(c_cmdname);
      FOR v_feature IN GetGeomBasedFCs LOOP
        v_table     := v_feature.table_name;
        v_geometry  := v_feature.column_name;
        i_srid      := GetSRID( v_table, v_geometry );
        IF i_dimension is NULL THEN
           i_dim    := GetDim( v_table, v_geometry );
        ELSE  
           i_dim    := i_dimension;
        END IF;
        IF i_srid IS NULL THEN 
          i_srid:=8307;
          RESPONSE( v_table, v_geometry||' SRID cannot be NULL: 8307 used as placeholder.');
          RESPONSE('WARNING','Change with EXEC GOOM.SETSRID('''|| v_table ||''','''|| v_geometry ||''',srid);');
        END IF;
        IF NOT isGeographic( i_srid ) THEN
          Response(c_cmdname||c_msgWarning,'SRID: '|| i_srid ||' indicates projected, incorrect extents assigned:'|| v_table ||'.'|| v_geometry);
        END IF;
        InsertMBR( i_dim, v_table, v_geometry, -180, 180, -90, 90, c_gtol, i_srid);
        Response( v_table, c_feedback1 || v_geometry);
      END LOOP;
      ProcessComplete( c_cmdname );
      WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_feedback1||USER);
    ELSE
      v_ownertable := GetOwnerObject( v_tablename);
      v_schema      := SplitOwnerObject( v_ownertable, 'OWNER');
      v_table      := SplitOwnerObject( v_ownertable, 'TABLE'); 
      IF NOT ChkTable( v_ownertable ) AND NOT ChkView( v_ownertable ) THEN
        RAISE e_TableNotFound;
      END IF;
      FOR v_geom IN (SELECT column_name FROM ALL_TAB_COLUMNS WHERE OWNER = v_schema
                     AND DATA_TYPE = 'SDO_GEOMETRY' AND TABLE_NAME = v_table) LOOP
        v_geometry := v_geom.column_name;
        IF i_dimension is NULL THEN
           i_dim := GetDim( v_table, v_geometry);
        ELSE  
           i_dim := i_dimension;
        END IF;
        i_srid   := GetSRID( v_ownertable, v_geometry);
        IF i_srid IS NULL THEN 
          i_srid:=8307;
          RESPONSE( c_cmdname || c_msgWarning,'SRID cannot be NULL: 8307 used as placeholder for: '|| v_ownertable ||'.'|| v_geometry );
          RESPONSE( c_cmdname || c_msgVerify ,'Set correct SRID with EXEC GOOM.SETSRID('''|| v_ownertable ||''','''|| v_geometry ||''', srid);');
        END IF;
        IF NOT isGeographic( i_srid ) THEN
          Response( c_cmdname || c_msgWarning,'SRID: ' || i_srid || ' indicates projected data in: ' || v_ownertable || '.' ||  v_geometry );
          Response( c_cmdname || c_msgVerify, 'Set correct extents using EXEC GOOM.SetMBRProj('''|| v_ownertable ||''');');
        ELSE
          Response( c_cmdname, c_feedback1 || v_ownertable || '.' || v_geometry);
          WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable, c_feedback1 || v_ownertable || '.' || v_geometry);
        END IF;
        InsertMBR( i_dim, v_ownertable, v_geometry, -180, 180, -90, 90, 0.01, i_srid);
      END LOOP;
    END IF;
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_ownertable, c_msgGoomInternal);
    WHEN OTHERS THEN
      ROLLBACK;
      REPORT_ERROR( c_cmdname, v_ownertable, v_debug, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geometry, c_msgOraError || SQLCODE);
  END SetMBRGeo;
  -----------------------------------------------------------------------------------------
  -- SetMBRProj set the default MBR in USER_SDO_GEOM_METADATA to the default for projected data
  -- +-2147483648, +-2147483648 for all feature classes.
  -- Syntax: EXEC GOOM.SetMBRProj or EXEC GOOM.SetMBRProj([v_tablename],[i_dimension]);
  --         i_dimension : the optional dimension to use, only specify this (2 or 3) if the geometry is empty.
  --
  PROCEDURE SetMBRProj( v_tablename IN VARCHAR2 DEFAULT 'ALL', i_dimension IN INTEGER DEFAULT NULL) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'SetMBRProj';
    c_cmdtype       CONSTANT VARCHAR2(4)  := 'MBR';
    c_appliesto     CONSTANT VARCHAR2(21) := 'All Feature Classes';
    c_feedback      CONSTANT VARCHAR2(55) := 'SRID Found, using default projected extents for: '; 
    --
    v_feature       GetGeomBasedFCs%ROWTYPE;
    v_geometry      COLS.COLUMN_NAME%TYPE;
    v_geom          COLS.COLUMN_NAME%TYPE;
    i_dim           INTEGER;
    i_srid          INTEGER;
    v_debug         VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_table         VARCHAR2(30);
    v_schema         VARCHAR2(30);
    --
  BEGIN
    IF v_tablename = 'ALL' THEN
      v_schema := USER;
      ProcessStart( c_cmdname );
      FOR v_feature IN GetGeomBasedFCs LOOP
        v_table     := v_feature.table_name;
        v_geometry  := v_feature.column_name;
        i_srid      := GetSRID( v_table, v_geometry);
        IF i_dimension is NULL THEN
           i_dim    := GetDim( v_table, v_geometry);
        ELSE  
           i_dim    := i_dimension;
        END IF;
        IF i_srid is NULL THEN
          Response( v_feature.table_name, c_feedback || v_geometry);        
        ELSIF isGeographic( i_srid ) THEN
          Response( v_feature.table_name,'SRID: '|| i_srid ||' is Geographic, geographic extents assigned.');
          SetMBRGeo( v_table, i_dim);
        ELSE
          InsertMBR( i_dim, v_table, v_geometry,c_XLO,c_XHI,c_YLO,c_YHI,c_tol,i_srid);       
        END IF;
      END LOOP;
      ProcessComplete( c_cmdname );
      WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_feedback||USER); 
    ELSE
      v_ownertable := GetOwnerObject( v_tablename);
      v_schema      := SplitOwnerObject( v_ownertable,'OWNER');
      v_table      := SplitOwnerObject( v_ownertable,'TABLE'); 
      IF NOT ChkTable( v_ownertable) AND NOT ChkView( v_ownertable) THEN
        RAISE e_TableNotFound;
      END IF;
      FOR v_geom IN (SELECT column_name FROM ALL_TAB_COLUMNS WHERE OWNER = v_schema
                     AND DATA_TYPE = 'SDO_GEOMETRY' AND TABLE_NAME = v_table) LOOP
        v_geometry := v_geom.column_name;
        i_srid     := GetSRID( v_ownertable, v_geometry);
        IF i_dimension is NULL THEN
           i_dim   := GetDim( v_table, v_geometry);
        ELSE  
           i_dim   := i_dimension;
        END IF;
        IF i_srid is NULL THEN
          SetMBR( v_ownertable, v_geometry); 
          Response( c_cmdname || c_msgVerify, c_msgSRIDIsNull || v_ownertable ||'.'|| v_geometry);       
        ELSIF isGeographic( i_srid ) THEN
          SetMBR( v_ownertable, v_geometry);
          Response( c_cmdname || c_msgWarning,'SRID: '|| i_srid ||' is geographic in '|| v_ownertable ||'.'|| v_geometry );
          Response( c_cmdname || c_msgVerify ,'Geographic extents were automatically assigned to: '|| v_ownertable ||'.'|| v_geometry );
        ELSE
          InsertMBR( i_dim, v_ownertable, v_geometry, c_XLO, c_XHI, c_YLO, c_YHI, c_tol, i_srid);
          Response( c_cmdname, c_feedback || v_ownertable);
          WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable, c_feedback|| v_ownertable || '.' || v_geometry);
        END IF;
      END LOOP;
    END IF;
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_ownertable, c_msgGoomInternal);
    WHEN OTHERS THEN
      ROLLBACK;
      REPORT_ERROR( c_cmdname, v_ownertable, v_debug, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geometry, c_msgOraError || SQLCODE);
  END SetMBRProj;
  ---------------------------------------------------------------------------------------
  -- CopyMBR copies the USER_SDO_GEOM_METADATA entries for one table/geometry to another one.
  -- Syntax: EXEC GOOM.CopyMBR(v_source_table,v_source_geom,v_target_table,v_target_geom);
  --         Input the source table/geometry and the target table/geometry.  You can also
  --         specify a view.
  --
  PROCEDURE CopyMBR( v_frmtable IN VARCHAR2, v_frmgeom IN VARCHAR2, v_totable IN VARCHAR2, v_togeom IN VARCHAR2) IS

    c_cmdname       CONSTANT VARCHAR2(32) := 'CopyMBR';
    c_cmdtype       CONSTANT VARCHAR2(4)  := 'MBR';
    c_feedback      CONSTANT VARCHAR2(25) := 'MBR already exists for: ';
    c_feedback2     CONSTANT VARCHAR2(16) := 'MBR copied to: ';
    --
    v_diminfo       MDSYS.SDO_DIM_ARRAY;
    i_srid          INTEGER;
    i_count         PLS_INTEGER:=0;
    v_sql           VARCHAR2(255);
    -- Added to support owner.table
    v_frmownertable        VARCHAR2(61);
    v_frmownername         VARCHAR2(30);
    v_frmtablename         VARCHAR2(30);
    v_toownertable         VARCHAR2(61);
    v_toownername          VARCHAR2(30);
    v_totablename          VARCHAR2(30);
  BEGIN
    -- Added to support owner.table
    v_frmownertable := GetOwnerObject( v_frmtable);
    v_frmownername  := SplitOwnerObject( v_frmownertable,'OWNER');
    v_frmtablename  := SplitOwnerObject( v_frmownertable,'TABLE'); 
    v_toownertable  := GetOwnerObject( v_totable);
    v_toownername   := SplitOwnerObject( v_toownertable,'OWNER');
    v_totablename   := SplitOwnerObject( v_toownertable,'TABLE');
    --
    v_sql:='SELECT DIMINFO FROM ALL_SDO_GEOM_METADATA WHERE OWNER = :vowner AND TABLE_NAME = :vfrmtable AND COLUMN_NAME = :vfrmgeom';
    EXECUTE IMMEDIATE v_sql INTO v_diminfo USING v_frmownername, v_frmtablename, v_frmgeom;
    v_sql:='SELECT SRID FROM ALL_SDO_GEOM_METADATA WHERE OWNER = :vowner AND TABLE_NAME = :vfrmtable AND COLUMN_NAME = :vfrmgeom';
    EXECUTE IMMEDIATE v_sql INTO i_srid USING v_frmownername, v_frmtablename, v_frmgeom;
    v_sql:='SELECT COUNT(1) FROM ALL_SDO_GEOM_METADATA WHERE OWNER = :vowner AND TABLE_NAME = :vtotable AND COLUMN_NAME = :vtogeom';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_toownername, v_toownername, v_togeom;
    --
    IF i_count = 0 THEN
      IF v_toownername = USER THEN
        v_sql:='INSERT INTO USER_SDO_GEOM_METADATA (TABLE_NAME, COLUMN_NAME, DIMINFO, SRID) 
                VALUES (:totable, :togeom, :DIMINFO, :SRID)';
        EXECUTE IMMEDIATE v_sql USING v_totablename, v_togeom, v_DIMINFO, i_srid;
        COMMIT;
        Response( c_cmdname, c_feedback2 || v_toownertable ||'.'|| v_togeom);
        WRITE_RESULTS( v_frmownername, c_cmdtype, c_cmdname, v_frmownertable ||'.'|| v_frmgeom, c_feedback2 || v_toownertable ||'.'|| v_togeom);
      ELSIF v_toownername !=USER AND chkInsertOnMDSYS THEN
        v_sql:='INSERT INTO MDSYS.SDO_GEOM_METADATA_TABLE (SDO_OWNER, SDO_TABLE_NAME, SDO_COLUMN_NAME, SDO_DIMINFO, SDO_SRID) 
                VALUES (:toownername, :totablename, :togeom, :DIMINFO, :SRID)';
        EXECUTE IMMEDIATE v_sql USING v_toownername, v_totablename, v_togeom, v_DIMINFO, i_srid;
        COMMIT;
        Response( c_cmdname, c_feedback2|| v_toownertable ||'.'|| v_togeom);
        WRITE_RESULTS( v_frmownername, c_cmdtype, c_cmdname, v_frmtable ||'.'|| v_frmgeom, c_feedback2 || v_toownertable ||'.'|| v_togeom);     
      ELSE
        RAISE e_NoMetadataAccess;
      END IF;
    ELSE
      Response( c_cmdname || c_msgWarning, c_feedback || v_toownertable || '.' || v_togeom || '.');
      WRITE_RESULTS( v_frmownername, c_cmdtype, c_cmdname, v_frmownertable || '.' || v_frmgeom, c_feedback || v_frmownertable || '.' || v_togeom);
    END IF;
  EXCEPTION 
    WHEN e_NoMetadataAccess THEN
      Response( c_cmdname || c_msgError, c_msgNoMetadataAccess);
      WRITE_RESULTS( v_frmownername,  c_msgError, c_cmdname, c_msgNoMetadataAccess, c_msgGoomInternal);
    WHEN OTHERS THEN
      ROLLBACK;
      REPORT_ERROR( c_cmdname, v_toownertable || '.' || v_togeom, v_frmownertable || '.' || v_frmgeom, v_sql, SQLERRM);
      WRITE_RESULTS( v_frmownername, c_msgError, c_cmdname, v_toownertable || '.' || v_togeom, c_msgOraError || SQLCODE);
  END CopyMBR;
  --------------------------------------------------------------------------------------
  -- DeleteOrphanMBR deletes orphan metadata from USER_SDO_GEOM_METADATA.  Orphans would 
  -- occur if you had entries for table/views that were no longer in the schema.
  -- Syntax: EXEC GOOM.DeleteOrphanMBR;
  --    
  PROCEDURE DeleteOrphanMBR ( v_schema IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(20) := 'DeleteOrphanMBR';
    c_cmdtype       CONSTANT VARCHAR2(3)  := 'MBR';
    c_feedback1     CONSTANT VARCHAR2(54) := 'Orphaned records deleted from USER_SDO_GEOM_METADATA: ';
    c_feedback2     CONSTANT VARCHAR2(61) := 'Orphaned records deleted from MDSYS.SDO_GEOM_METADATA_TABLE: ';
    c_feedback3     CONSTANT VARCHAR2(45) := 'No orphan metadata records found in schema:';
    --
    v_feature       GetTableGeomsFromUserMBR%ROWTYPE; -- Table.column being processed
    i_count         PLS_INTEGER  :=0; -- Results of existence check
    i_delcount      PLS_INTEGER  :=0;
    v_sql           VARCHAR2(255);
    i_cur           PLS_INTEGER;
    i_ret           PLS_INTEGER;
    v_dbtab         VARCHAR2(30);
    v_dbcol         VARCHAR2(61);
  BEGIN
    IF v_schema = USER THEN
      FOR v_feature  IN GetTableGeomsFromUserMBR LOOP
        -- Look at all entries in metadata using global cursor
        v_sql:='SELECT COUNT(1) FROM ALL_TAB_COLUMNS WHERE OWNER=:vowner AND TABLE_NAME = :vtablename AND COLUMN_NAME = :vgeometry';
        IF v_feature.column_name = 'GEORASTER.SPATIALEXTENT' THEN -- Check for georaster
          EXECUTE IMMEDIATE v_sql INTO i_count USING USER, v_feature.table_name, 'GEORASTER';
        ELSE
          EXECUTE IMMEDIATE v_sql INTO i_count USING USER, v_feature.table_name, v_feature.column_name;
        END IF;
        IF i_count = 0 THEN        -- If count=0, metadata entry does not exist in schema.
           v_sql:='DELETE FROM USER_SDO_GEOM_METADATA WHERE TABLE_NAME = :vtablename AND COLUMN_NAME = :vgeometry';
           IF v_feature.column_name = 'GEORASTER.SPATIALEXTENT' THEN -- Check for georaster
             EXECUTE IMMEDIATE v_sql USING v_feature.table_name, 'GEORASTER';
           ELSE
             EXECUTE IMMEDIATE v_sql USING v_feature.table_name, v_feature.column_name;
           END IF;
           i_delcount := i_delcount+1;
        END IF;
      END LOOP;
      IF i_delcount = 0 THEN
        -- If no changes are made, let the user know
        Response( c_cmdname, c_feedback3 || USER);
      ELSE
        Response( c_cmdname, c_feedback1 || i_delcount);
        WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_feature.table_name ||'.'|| v_feature.column_name, c_feedback1 || i_delcount);
      END IF;
    ELSE
      IF NOT chkInsertOnMDSYS THEN
        RAISE e_NoMetadataAccess;
      ELSE
        v_sql := 'SELECT SDO_TABLE_NAME, SDO_COLUMN_NAME FROM MDSYS.SDO_GEOM_METADATA_TABLE WHERE SDO_OWNER ='''|| v_schema||'''';
        --
        i_cur := SYS.DBMS_SQL.OPEN_CURSOR;
        SYS.DBMS_SQL.PARSE( i_cur, v_sql, SYS.DBMS_SQL.NATIVE);
        SYS.DBMS_SQL.DEFINE_COLUMN( i_cur, 1, v_dbtab,30);
        SYS.DBMS_SQL.DEFINE_COLUMN( i_cur, 2, v_dbcol,61);
        i_ret := SYS.DBMS_SQL.EXECUTE( i_cur );
        LOOP
          IF (SYS.DBMS_SQL.FETCH_ROWS( i_cur ) = 0) THEN
              EXIT;
          END IF;
          SYS.DBMS_SQL.COLUMN_VALUE( i_cur, 1, v_dbtab);
    	  SYS.DBMS_SQL.COLUMN_VALUE( i_cur, 2, v_dbcol);
          v_sql:='SELECT COUNT(1) FROM ALL_TAB_COLUMNS WHERE OWNER=:sdo_owner AND TABLE_NAME = :table_name AND COLUMN_NAME = :sdo_geometry';
          IF v_dbcol = 'GEORASTER.SPATIALEXTENT' THEN -- Check for georaster
            EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_dbtab, 'GEORASTER';
          ELSE
            EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_dbtab, v_dbcol;
          END IF;
          IF i_count = 0 THEN        -- If count=0, metadata entry does not exist in schema.
             v_sql:='DELETE FROM MDSYS.SDO_GEOM_METADATA_TABLE WHERE SDO_OWNER=:sdo_owner AND SDO_TABLE_NAME=:sdo_table_name AND SDO_COLUMN_NAME=:sdo_column_name';
             IF v_dbcol = 'GEORASTER.SPATIALEXTENT' THEN -- Check for georaster
               EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_dbtab, 'GEORASTER';
             ELSE                 
               EXECUTE IMMEDIATE v_sql USING v_schema, v_dbtab, v_dbcol;
             END IF;
             i_delcount := i_delcount+1;
          END IF;
        END LOOP;
        SYS.DBMS_SQL.CLOSE_CURSOR( i_cur );
        IF i_delcount = 0 THEN
          -- If no changes are made, let the user know
          Response(c_cmdname,c_feedback3||v_schema);
        ELSE
          Response( c_cmdname, c_feedback2 || i_delcount);
          WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, USER, c_feedback2 || i_delcount);
        END IF;
      END IF;
    END IF;
  EXCEPTION
    WHEN e_NoMetadataAccess THEN
      Response( c_cmdname || c_msgError, c_msgNoMetadataAccess || USER);
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      REPORT_ERROR( c_cmdname, v_schema, v_sql, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_sql, c_msgOraError || SQLCODE);
  END DeleteOrphanMBR;  
  --------------------------------------------------------------------------------------
  -- SetSRID sets the SRID value for the specified table/geometry to the specified SRID.
  -- SRID's are not required for projected data but they are required for geodetic data.
  -- Syntax: EXEC GOOM.SetSRID(v_tablename,v_geomcol,i_srid)
  -- Note:   i_srid=0 sets NULL value (Default) generally used for projected data.
  --
  PROCEDURE SetSRID( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, i_srid IN INTEGER DEFAULT 0) IS
    c_cmdname CONSTANT VARCHAR2(32) := 'SetSRID';
    c_cmdtype CONSTANT VARCHAR2(3)  := 'MBR';
    v_sql     VARCHAR2(255);
    v_geom    VARCHAR2(30);
    -- Added for owner.table support
    v_ownertable         VARCHAR2(61);
    v_schema             VARCHAR2(30);
    v_table              VARCHAR2(30);
  BEGIN
    -- Added for owner.table support
    v_ownertable := GetOwnerObject( v_tablename );
    v_schema      := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      := SplitOwnerObject( v_ownertable, 'TABLE'); 
    --
    IF NOT ChkTable( v_ownertable ) AND NOT ChkView( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF NOT ChkGeometry( v_ownertable, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    -- Set the SRID in 2 places, USER_SDO_GEOM_METADATA and in the SDO_GEOMETRY
    IF i_srid = 0 THEN                                      -- allow for the setting of a NULL SRID
      IF v_schema = USER THEN                                -- if the user is the table owner, process normally
        DropSidx( v_ownertable, v_geom);                     -- First drop existing spatial index
        v_sql := 'UPDATE USER_SDO_GEOM_METADATA SET SRID=NULL WHERE TABLE_NAME=:vtablename AND COLUMN_NAME=:vgeometry';
        EXECUTE IMMEDIATE v_sql USING v_table, v_geom;       -- Set USER_SDO_GEOM_METADATA SRID=NULL
        COMMIT;
      ELSIF v_schema != USER and chkInsertOnMDSYS THEN       -- if user is not owner, check for privileges to process
        DropSidx( v_ownertable, v_geom);
        v_sql := 'UPDATE MDSYS.SDO_GEOM_METADATA_TABLE SET SDO_SRID=NULL WHERE SDO_OWNER=:vowner AND SDO_TABLE_NAME=:vtablename AND SDO_COLUMN_NAME=:vgeometry';
        EXECUTE IMMEDIATE v_sql USING v_schema, v_table, v_geom; -- Set MDSYS.SDO_GEOM_METADATA_TABLE SDO_SRID=NULL
        COMMIT;
      ELSE                                                  -- no privileges so raise exception
        RAISE e_NoMetadataAccess;
      END IF;
      IF NOT chkView( v_ownertable ) THEN                    -- process table but check for view
        v_sql := 'update ' || v_ownertable || ' A set A.' || v_geom || '.SDO_SRID=NULL WHERE A.' || v_geom ||' IS NOT NULL';
        EXECUTE IMMEDIATE v_sql;                            -- Set the SDO_GEOMETRY.SDO_SRID=NULL
        COMMIT;
        Response(c_cmdname,'SRID set to NULL for: ' || v_ownertable || '.' || v_geom);
        WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable || '.' || v_geom, 'Updated SRID to NULL');
      END IF;
    ELSE                                                    -- Set an actual value for the SRID
      IF v_schema = USER THEN
        DropSidx( v_ownertable, v_geom);
        v_sql := 'UPDATE USER_SDO_GEOM_METADATA SET SRID=:vsrid where table_name=:vtablename and column_name=:vgeometry';
        EXECUTE IMMEDIATE v_sql USING i_srid, v_table, v_geom;     -- Set USER_SDO_GEOM_METADATA.SRID=i_srid
        COMMIT; 
      ELSIF v_schema != USER and chkInsertOnMDSYS THEN              -- if the user is the table owner, process normally
        DropSidx( v_ownertable, v_geom);
        v_sql := 'UPDATE MDSYS.SDO_GEOM_METADATA_TABLE SET SDO_SRID=:vsrid WHERE SDO_OWNER=:vowner AND SDO_TABLE_NAME=:vtablename AND SDO_COLUMN_NAME=:vgeometry';
        EXECUTE IMMEDIATE v_sql USING i_srid, v_schema, v_table, v_geom; -- Set MDSYS.SDO_GEOM_METADATA_TABLE SDO_SRID=NULL
        COMMIT;
      ELSE
        RAISE e_NoMetadataAccess;                                  -- no privileges so raise exception.
      END IF;
      IF NOT chkView( v_ownertable ) THEN                            -- process table but check for view
        v_sql := 'update ' || v_table || ' A SET A.' || v_geom || '.SDO_SRID=' || i_srid || ' WHERE A.' || v_geom || ' IS NOT NULL';
        EXECUTE IMMEDIATE v_sql; -- Set SDO_GEOMETRY.SDO_SRID=i_srid
        COMMIT;
        Response( c_cmdname,'SRID set to ' || i_srid || ' for: ' || v_ownertable || '.' || v_geom);
        WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable || '.' || v_geom, 'Updated SRID to ' || i_srid);
      END IF;
    END IF;
    Rtree( v_ownertable, v_geom);                           -- Re-index the geometry after setting SRID
  EXCEPTION
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_ownertable || '.' || v_geom);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geom, c_msgGoomInternal);
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable, c_msgGoomInternal);
    WHEN e_NoMetadataAccess THEN
      Response( c_cmdname || c_msgError, c_msgNoMetadataAccess || USER);
  	  Response( c_cmdname || c_msgError,'Connect to '||v_schema||' and run');
      Response( c_cmdname || c_msgError,'EXEC GOOM.SetSRID('''|| v_table ||''','''|| v_geom||''','''|| i_srid ||');');
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgNoMetadataAccess || USER, c_msgGoomInternal);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geom, v_sql, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geom, c_msgOraError || SQLCODE);
  END SetSRID;
  --------------------------------------------------------------------------------------
  -- SetSRIDAll sets the specified SRID for all tables/geometries in the schema.
  -- Syntax: EXEC GOOM.SetSRIDAll(i_srid, [v_schema]);
  --         v_schema : optional and can be used by a DBA to operate on any schema.
  -- Note:   i_srid=0 sets NULL value (Default) generally used for projected data.
  -- 
  PROCEDURE SetSRIDAll( i_srid IN INTEGER DEFAULT 0, v_schema in VARCHAR2 DEFAULT USER) is
  c_cmdname     CONSTANT VARCHAR2(10) := 'SetSRIDAll';
  c_cmdtype     CONSTANT VARCHAR2(8)  := 'METADATA';
  c_param       CONSTANT VARCHAR2(6)  := 'SRID: ';
  --
  v_feature     GetGeomBasedFCs%ROWTYPE;
  BEGIN
  WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_msgStart || v_schema, c_param||i_srid);
  Response( c_cmdname, c_msgStart || v_schema,28);
    FOR v_feature in GetGeomBasedFCs( v_schema ) LOOP
      SETSRID( v_schema ||'.'|| v_feature.table_name, v_feature.column_name, i_srid);
    END LOOP;
  Response( c_cmdname, c_msgComplete || v_schema,28);
  WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_msgComplete || v_schema, c_param || i_srid);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_schema ||'.'|| v_feature.table_name ||'.'|| v_feature.column_name, i_srid, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_schema ||'.'|| v_feature.table_name ||'.'|| v_feature.column_name, c_msgOraError || SQLCODE);
  END SetSRIDAll;
  ---------------------------------------------------------------------------------------
  -- SetSpatialTolerance sets the Oracle spatial tolerance to use for the specified table/
  -- geometry (USER_SDO_GEOM_METADATA).  The default tolerance is 0.00005 m and is the 
  -- recommended value to use for projected data.
  -- Syntax: EXEC GOOM.SetSpatialTolerance(v_tablename,v_geometry,n_tol); 
  --
  PROCEDURE SetSpatialTolerance( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, 
                                 n_tol       IN NUMBER DEFAULT 0.00005) IS
    c_cmdname   CONSTANT VARCHAR2(19):='SetSpatialTolerance';
    c_cmdtype   CONSTANT VARCHAR2(8):='METADATA';
    v_diminfo   mdsys.sdo_dim_array;
    v_sql       VARCHAR2(255);
    v_debug     VARCHAR2(255);
    v_geom      VARCHAR2(30);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    v_table         VARCHAR2(30);
    --
    i_dim           PLS_INTEGER;
  BEGIN
    --
    v_ownertable := GetOwnerObject( v_tablename);
    v_schema      := SplitOwnerObject( v_ownertable,'OWNER');
    v_table      := SplitOwnerObject( v_ownertable,'TABLE'); 
    --
    IF NOT ChkTable( v_ownertable )THEN
      RAISE e_TableNotFound;
    END IF;
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF NOT ChkGeometry( v_ownertable, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    i_dim :=GetDIM( v_ownertable, v_geom);
    IF v_schema=USER THEN
      v_sql := 'SELECT MD.DIMINFO FROM USER_SDO_GEOM_METADATA MD WHERE TABLE_NAME=:vtable AND COLUMN_NAME=:vcolumn FOR UPDATE';
      EXECUTE IMMEDIATE v_sql INTO v_diminfo USING v_table, v_geom;
    ELSE
      IF chkInsertOnMDSYS THEN
        v_sql := 'SELECT MD.SDO_DIMINFO FROM MDSYS.SDO_GEOM_METADATA_TABLE MD WHERE SDO_OWNER=:vowner AND SDO_TABLE_NAME=:vtable AND SDO_COLUMN_NAME=:vcolumn';
        EXECUTE IMMEDIATE v_sql INTO v_diminfo USING v_schema, v_table, v_geom;
      ELSE
        v_sql := 'SELECT MD.DIMINFO FROM ALL_SDO_GEOM_METADATA MD WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND COLUMN_NAME=:vcolumn';
        EXECUTE IMMEDIATE v_sql INTO v_diminfo USING v_schema, v_table, v_geom;
      END IF;
    END IF;
    IF i_dim = 2 THEN
      v_diminfo(1).sdo_tolerance := n_tol;
      v_diminfo(2).sdo_tolerance := n_tol;
    ELSIF i_dim = 3 THEN
      v_diminfo(1).sdo_tolerance := n_tol;
      v_diminfo(2).sdo_tolerance := n_tol;
      v_diminfo(3).sdo_tolerance := n_tol;
    ELSE
      RAISE e_InvalidDimension;
    END IF;
    --
    IF v_schema = USER THEN
      v_sql := 'UPDATE USER_SDO_GEOM_METADATA A SET A.DIMINFO =:diminfo WHERE TABLE_NAME=:vtable AND COLUMN_NAME=:vcolumn';
      EXECUTE IMMEDIATE v_sql USING v_diminfo, v_table, v_geom;
      COMMIT;
    ELSIF v_schema != USER AND chkInsertOnMDSYS THEN
      v_sql := 'UPDATE MDSYS.SDO_GEOM_METADATA_TABLE A SET A.SDO_DIMINFO =:diminfo 
                 WHERE SDO_OWNER=:vowner AND SDO_TABLE_NAME=:vtable AND SDO_COLUMN_NAME=:vcolumn';
      EXECUTE IMMEDIATE v_sql USING v_diminfo, v_schema, v_table, v_geom;
      COMMIT;
    ELSE
      RAISE e_NoMetadataAccess;
    END IF;
    RTREE( v_tablename, v_geomcol);
    Response( c_cmdname,'Oracle tolerance for '|| v_ownertable ||'.'|| v_geom ||' set to '|| n_tol ||'.');
    WRITE_RESULTS( v_schema, c_cmdname, c_cmdtype, 'Oracle tolerance for '|| v_ownertable ||'.'|| v_geom ||' set to '|| n_tol||'.', c_msgOraError || SQLCODE);
  EXCEPTION
    WHEN e_NoMetadataAccess THEN
      Response( c_cmdname || c_msgError, c_msgNoMetadataAccess || USER); 
    WHEN e_InvalidDimension THEN
      Response( c_cmdname || c_msgError, c_msgInvalidDimension);
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_ownertable ||'.'|| v_geom);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgGeometryNotFound || v_geomcol, c_msgGoomInternal);
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_ownertable, c_msgGoomInternal);
    WHEN OTHERS THEN
      WRITE_RESULTS( v_schema, c_cmdname, c_cmdtype, 'Oracle tolerance for '|| v_ownertable ||'.'||v_geom||' set to '|| n_tol ||'.', c_msgOraError || SQLCODE);
      REPORT_ERROR ( c_cmdname, v_sql, v_debug,sqlcode,sqlerrm);
  END SetSpatialTolerance;
  -- -----------------------------------------------------------------------------------
  -- SetSpatialToleranceAll sets the Oracle spatial tolerance for all table/geometries in
  -- the schema.  The default tolerance is 0.00005 m and is the recommended value to use for 
  -- projected data.
  -- Syntax: EXEC GOOM.SetSpatialToleranceAll(n_tol, [v_schema]);
  --         v_schema : optional and can be used by a DBA to operate on any schema.
  --
  PROCEDURE SetSpatialToleranceAll( n_tol IN NUMBER DEFAULT 0.00005, v_schema IN VARCHAR2 DEFAULT USER) is
  c_cmdname     CONSTANT VARCHAR2(22) := 'SetSpatialToleranceAll';
  c_cmdtype     CONSTANT VARCHAR2(8)  := 'METADATA';
  c_param       CONSTANT VARCHAR2(11) := 'Tolerance: ';
  --
  v_feature     GetGeomBasedFCs%ROWTYPE;
  BEGIN
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_msgStart || v_schema, c_param || n_tol);
    Response( c_cmdname, c_msgStart|| v_schema,28);
    FOR v_feature in GetGeomBasedFCs( v_schema ) LOOP
      SetSpatialTolerance( v_schema ||'.'|| v_feature.table_name, v_feature.column_name, n_tol);
    END LOOP;
    Response( c_cmdname, c_msgComplete||v_schema,28);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_msgComplete || v_schema, c_param||n_tol);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_schema ||'.'|| v_feature.table_name ||'.'|| v_feature.column_name, n_tol, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_schema||'.'|| v_feature.table_name ||'.'|| v_feature.column_name, c_msgOraError || SQLCODE);
  END SetSpatialToleranceAll;
  ---------------------------------------------------------------------------------------
  -- GetSpatialTolerance returns the current tolerance setting for the table/geometry.
  -- Syntax: SELECT GOOM.GetSpatialTolerance(v_tablename,v_geometry,[v_dim]) FROM DUAL; 
  --         c_dim : optional and can be 'X', 'Y' or 'Z' if tolerances are different.
  --
  FUNCTION GetSpatialTolerance( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, 
                                v_dim       IN VARCHAR2 DEFAULT 'X') RETURN NUMBER IS
    c_cmdname     CONSTANT VARCHAR2(24):='GetSpatialTolerance';
    --
    v_diminfo     MDSYS.SDO_DIM_ARRAY;
    v_sql         VARCHAR2(255);
    n_tol         NUMBER:=0;
    v_geom        VARCHAR2(30);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    v_table         VARCHAR2(30);
    --
  BEGIN
    --
    v_ownertable := GetOwnerObject( v_tablename);
    v_schema      := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      := SplitOwnerObject( v_ownertable, 'TABLE'); 
    --
    IF NOT ChkTable( v_ownertable)THEN
      RAISE e_TableNotFound;
    END IF;
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF NOT ChkGeometry( v_ownertable, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    v_sql:='SELECT MD.DIMINFO FROM ALL_SDO_GEOM_METADATA MD WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND COLUMN_NAME=:vcolumn ';
    EXECUTE IMMEDIATE v_sql INTO v_diminfo USING v_schema, v_table, v_geom;
    CASE v_dim 
       WHEN 'X' THEN 
         n_tol := v_diminfo(1).sdo_tolerance;
       WHEN 'Y' THEN 
         n_tol := v_diminfo(2).sdo_tolerance;
       WHEN 'Z' THEN 
         n_tol := v_diminfo(3).sdo_tolerance;
       ELSE 
         n_tol := v_diminfo(1).sdo_tolerance;
    END CASE;
    RETURN n_tol;
  EXCEPTION
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound|| v_ownertable ||'.'|| v_geom);
      RETURN 0;
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_ownertable);
      RETURN 0;
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, v_sql, n_tol,sqlcode,sqlerrm);
      RETURN 0;
  END GetSpatialTolerance;  

  -- -----------------------------------------------------------------------------------------------------------------
  -- SPATIAL TUNING PROCEDURES: Spatial indexing
  -- -----------------------------------------------------------------------------------------------------------------

  -- DelSIDX deletes all the spatial indexes in the specified schema.
  -- Syntax: EXEC GOOM.DelSIDX; -- for the current schema.
  --         EXEC GOOM.DelSIDX(v_schema); -- as a DBA operation.
  --
  PROCEDURE DelSidx ( v_schema IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(8) := 'DelSIDX';
    c_cmdtype       CONSTANT VARCHAR2(6) := 'TUNING';
    c_appliesto     CONSTANT VARCHAR2(18):= 'ALL DOMAIN INDEXES';
    c_feedback      CONSTANT VARCHAR2(38):= 'Deleted all spatial indexes owned by: ';
    --
    v_index         GetSpatialIndexNames%ROWTYPE;
    v_sql           VARCHAR2(255);
    v_ownerindex    VARCHAR2(61);
  BEGIN
    FOR v_index IN GetSpatialIndexNames( v_schema ) LOOP
      v_ownerindex := v_schema||'.'|| v_index.index_name;
      v_sql := 'DROP INDEX ' || v_ownerindex || ' FORCE';
      EXECUTE IMMEDIATE v_sql;
    END LOOP;
    RESPONSE( c_cmdname, c_feedback || v_schema);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_feedback || v_schema);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_index.index_name, v_sql, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_index.index_name, c_msgOraError || SQLCODE);
  END DelSidx;
  --------------------------------------------------------------------------------------
  -- DropSidx drops the spatial index on the specified table/geometry pair.
  -- EXEC GOOM.DropSIDX(v_table_name,v_column_name)
  --
  PROCEDURE DropSidx( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) IS
    c_cmdname       CONSTANT VARCHAR2(8) := 'DropSIDX';
    c_cmdtype       CONSTANT VARCHAR2(6) := 'TUNING';
    c_feedback      CONSTANT VARCHAR2(26):= ' - Spatial index deleted: ';
    c_feedback2     CONSTANT VARCHAR2(50):= 'The spatial index cannot be found for this table.';
    --
    v_index         USER_SDO_INDEX_INFO.INDEX_NAME%TYPE;
    v_sql           VARCHAR2(255);
    v_geom          VARCHAR2(30);
    i_count         PLS_INTEGER := 0;
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    v_table         VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownertable:=GetOwnerObject( v_tablename);
    v_schema    :=SplitOwnerObject( v_ownertable, 'OWNER');
    v_table     :=SplitOwnerObject( v_ownertable, 'TABLE');
    -- Check Inputs
    IF NOT ChkTable( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    -- Determine geometry column
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    -- Verify geometry column
    IF NOT ChkGeometry( v_ownertable, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    -- Check spatial index existence
    IF chkSpatialIndex( v_ownertable, v_geom) THEN
      -- Need to isolate the handling of good versus bad spatial indexes.
      BEGIN
        v_sql:='SELECT COUNT(INDEX_NAME) FROM ALL_SDO_INDEX_INFO WHERE TABLE_OWNER=:vowner AND COLUMN_NAME=:vcol AND TABLE_NAME=:vtab AND SDO_INDEX_OWNER=:vowner';
        EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_geom, v_table, v_schema;
        IF i_count > 1 AND NOT chkTablePartition( v_ownertable )  THEN
          Raise e_InvalidRowCount;
        END IF;
        v_sql:='SELECT DISTINCT(INDEX_NAME) FROM ALL_SDO_INDEX_INFO WHERE TABLE_OWNER=:vowner AND COLUMN_NAME=:vcol AND TABLE_NAME=:vtab AND SDO_INDEX_OWNER=:vowner';
        EXECUTE IMMEDIATE v_sql INTO v_index USING v_schema, v_geom, v_table, v_schema;
      EXCEPTION
        WHEN e_NoDataFound THEN
          v_sql:='SELECT INDEX_NAME FROM ALL_INDEXES WHERE OWNER=:vowner AND TABLE_NAME=:vtab AND DOMIDX_OPSTATUS=''FAILED''';
          EXECUTE IMMEDIATE v_sql INTO v_index USING v_schema, v_table;
        WHEN e_InvalidRowCount THEN
          REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geom, v_sql, SQLCODE, SQLERRM);
          RESPONSE( c_cmdname || c_msgError, c_msgInvalidRowCount );
      END;
      -- 
      IF v_index IS NULL THEN
        RAISE e_NoDataFound;
      END IF;
      v_sql := 'DROP INDEX ' || v_schema || '.' || v_index || ' FORCE';
      EXECUTE IMMEDIATE v_sql;
      Response( c_cmdname, v_ownertable ||'.'|| v_geom || c_feedback || v_index);
      WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable || '.' || v_geom, c_feedback || v_index);
    ELSE
      Response( c_cmdname || c_msgWarning, c_msgNoSIDX || v_ownertable ||'.'|| v_geom);
    END IF;
  EXCEPTION
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_ownertable ||'.'|| v_geom);
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_ownertable);
    WHEN e_NoDataFound THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geom, c_feedback2, SQLCODE, SQLERRM);
    WHEN e_InvalidRowCount THEN
      Response( c_cmdname || c_msgError, c_msgInvalidRowCount || v_ownertable ||'.'|| v_geom);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geom, v_sql, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geom, c_msgOraError || SQLCODE);
  END DropSidx;
  --------------------------------------------------------------------------------------  
  -- Rtree is the internal rtree indexing procedure.  It is meant for GOOM use only.
  --
  PROCEDURE RTree( v_tablename IN VARCHAR2, 
                     v_geomcol IN VARCHAR2 DEFAULT NULL, 
                     b_geomopt IN BOOLEAN DEFAULT TRUE,
                       b_stats IN BOOLEAN DEFAULT TRUE) IS
    c_cmdname       CONSTANT VARCHAR2(14):= 'RTree Indexing';
    c_cmdtype       CONSTANT VARCHAR2(6) := 'TUNING';
    c_feedback1     CONSTANT VARCHAR2(26):= 'Created spatial index on: ';
    c_feedback2     CONSTANT VARCHAR2(38):= 'Created partitioned spatial index on: ';
    --
    v_geomtype      VARCHAR2(16)         := 'UNKNOWN';
    v_geom          VARCHAR2(30);
    v_index         VARCHAR2(61);
    v_indexspace    VARCHAR2(30);
    v_sql           VARCHAR2(512);
    v_ownertable    VARCHAR2(61); 
    v_schema         VARCHAR2(30);
    --
  BEGIN
    v_ownertable := GetOwnerObject( v_tablename);
    v_schema      := SplitOwnerObject( v_ownertable, 'OWNER');
    v_indexspace := GetGOOMIndexSpace;
    -- Check Inputs
    -- If it is a view, raise exception.
    IF chkVIEW( v_ownertable ) THEN
      RAISE e_TableIsView;
    END IF;
    -- If the table does not exist, raise exception.
    IF NOT chkTable( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    -- If the geometry is not specified, find it.
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    -- If the geometry does not exist, raise exception.
    IF NOT ChkGeometry( v_ownertable, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    -- Delete the current spatial index if it exists.
    IF chkSpatialIndex( v_ownertable, v_geom) THEN
      DropSidx( v_ownertable, v_geom);
    END IF;
    -- Get the geometry type if b_geomopt is true, otherwise geometry is unkown. 
    IF b_geomopt THEN
      v_geomtype := GetGeomType( v_ownertable, v_geom);
    ELSE
      v_geomtype := 'UNKNOWN';
    END IF;
    -- Get a unique index name to use.
    v_index      := GetINDXNAME( v_ownertable);
    -- Handle the indexing here...   
    -- If the table is partitioned, indexing uses a LOCAL key word.
    IF chkTablePartition( v_ownertable ) THEN
      IF v_geomtype = 'UNKNOWN' OR isText( v_ownertable, v_geom) THEN
        v_sql := 'CREATE INDEX ' || v_index || ' ON ' || v_ownertable || '(' || v_geom ||
                 ') INDEXTYPE IS MDSYS.SPATIAL_INDEX PARAMETERS(''SDO_NON_LEAF_TBL=TRUE TABLESPACE=' || v_indexspace || ''') LOCAL';
        EXECUTE IMMEDIATE v_sql;
        Response( c_cmdname, c_feedback2 || v_ownertable || '.' || v_geom ||' in '|| v_indexspace ||'.');
      ELSE
        v_sql := 'CREATE INDEX ' || v_index || ' ON ' || v_ownertable || '(' || v_geom ||
                 ') INDEXTYPE IS MDSYS.SPATIAL_INDEX PARAMETERS(''LAYER_GTYPE='|| v_geomtype ||' SDO_NON_LEAF_TBL=TRUE TABLESPACE=' || v_indexspace || ''') LOCAL';
        EXECUTE IMMEDIATE v_sql;
        Response( c_cmdname, c_feedback2 || v_ownertable || '.' || v_geom ||' in '|| v_indexspace ||' optimized for '|| v_geomtype ||'.');
      END IF;
    ELSE      -- If the table is not partitioned, its gets a normal spatial index.
      IF v_geomtype = 'UNKNOWN' OR isText( v_ownertable, v_geom) THEN
        v_sql := 'CREATE INDEX ' || v_index || ' ON ' || v_ownertable || '(' || v_geom ||
                 ') INDEXTYPE IS MDSYS.SPATIAL_INDEX PARAMETERS('' TABLESPACE=' || v_indexspace || ''')';
        EXECUTE IMMEDIATE v_sql;
        Response( c_cmdname, c_feedback1|| v_ownertable || '.' || v_geom ||' in '|| v_indexspace ||'.');
      ELSE
        v_sql := 'CREATE INDEX ' || v_index || ' ON ' || v_ownertable || '(' || v_geom ||
                 ') INDEXTYPE IS MDSYS.SPATIAL_INDEX PARAMETERS(''LAYER_GTYPE='|| v_geomtype ||' TABLESPACE=' || v_indexspace || ''')';
        EXECUTE IMMEDIATE v_sql;
        Response( c_cmdname, c_feedback1|| v_ownertable || '.' || v_geom ||' in '|| v_indexspace ||' optimized for '|| v_geomtype ||'.');
      END IF;
    END IF;
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable || '.' || v_geom, 'Spatially Indexed');
    -- Gather statistics for the new index.
    -- Replaced Analyze, depracated by Oracle (06/13/2015)
    -- Added option to skip stats
    IF b_stats THEN
      BEGIN 
        DBMS_STATS.GATHER_INDEX_STATS( v_schema, SplitOwnerObject( v_index,'OBJECT' ));
        Response( c_cmdname, 'Statistics gathered for: '|| v_index);
      EXCEPTION
        WHEN OTHERS THEN
          Response( c_cmdname, 'Statistics failed for: '|| v_index);
      END;
    ELSE
      Response( c_cmdname, 'No statistics were gathered for: '|| v_index);
    END IF;
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable, c_msgGoomInternal);
    WHEN e_TableIsView THEN
      Response( c_cmdname || c_msgInform, c_msgTableIsView || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgInform, c_cmdname, c_msgTableIsView || v_ownertable, c_msgGoomInternal);
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound|| v_ownertable ||'.'|| v_geom);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geom, c_msgGoomInternal);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geom, v_sql, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geom, c_msgOraError || SQLCODE);
      IF chkSpatialIndex( v_ownertable, v_geom) THEN
        DropSidx( v_ownertable, v_geom);
      END IF; 
  END RTree;
  --------------------------------------------------------------------------------------
  -- SpatialIndex will create a spatial index on all geometry columns in the specified table.
  -- Syntax:  EXEC GOOM.SpatialIndex(v_tablename, [b_geomopt]);
  --          b_geomopt will create a geometry optimized index by default.  Set to FALSE if
  --          you do not want the geometry to be optimized.  Optimization constrains the 
  --          GTYPE so no other GTYPE can be introduced into the geometry.
  --
  PROCEDURE SpatialIndex( v_tablename IN VARCHAR2, 
                            b_geomopt IN BOOLEAN DEFAULT TRUE,
                            b_stats   IN BOOLEAN DEFAULT TRUE) IS
  --
  CURSOR GetRTreeList( v_schema VARCHAR2 DEFAULT USER, v_table VARCHAR2) IS
         Select COLUMN_NAME FROM ALL_SDO_GEOM_METADATA WHERE OWNER = v_schema AND TABLE_NAME = v_table;
    --
    c_cmdname     CONSTANT VARCHAR2(12):='SpatialIndex';
    c_feedback    CONSTANT VARCHAR2(77):='Run EXEC GOOM.SetMBR (c_ownertable,c_geomcol); for every geometry column in: ';
    v_col         GetRTreeList%Rowtype;
    v_ownertable  VARCHAR2(61);
    v_schema      VARCHAR2(30);
    v_table       VARCHAR2(30);
    i_count       PLS_INTEGER;
  BEGIN
    -- added for owner.table support
    v_ownertable  := GetOwnerObject( v_tablename);
    v_schema       := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table       := SplitOwnerObject( v_ownertable, 'TABLE');
    -- Check that the MBR metadata exists or cursor will not work.
    SELECT COUNT(*) INTO i_count FROM ALL_SDO_GEOM_METADATA WHERE OWNER = v_schema AND TABLE_NAME = v_table;
    IF i_count  < 1 THEN
     RAISE e_NoMetadataFound; -- No Oracle metadata found.
    END IF;
    -- Loop through geometry columns for the specified table and pass to RTree indexer.
    FOR v_col IN GetRTreeList( v_schema, v_table) LOOP
  	  RTREE( v_ownertable, v_col.column_name, b_geomopt, b_stats);
  	END LOOP;
  EXCEPTION
    WHEN e_NoMetadataFound THEN
      Response( c_cmdname || c_msgError, c_msgNoMetadataFound|| v_ownertable);
      Response( c_cmdname || c_msgSolution, c_feedback || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable, c_msgGoomInternal);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, v_col.column_name, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable, c_msgOraError || SQLCODE);
  END SpatialIndex;
  --------------------------------------------------------------------------------------
  -- SpatialIndexAll will create a spatial index for all geometry based columns in the 
  -- schema.  A DBA can specify any schema for this operation, otherwise it operates on 
  -- the current schema.
  -- Syntax:  EXEC GOOM.SpatialIndexAll([c_owner],[b_geomopt]);
  --          b_geomopt will create a geometry optimized index by default.  Set to FALSE if
  --          you do not want the geometry to be optimized.  Optimization constrains the 
  --          GTYPE so no other GTYPE can be introduced into the geometry.
  --
  PROCEDURE SpatialIndexAll( v_schema IN VARCHAR2 DEFAULT USER, 
                            b_geomopt IN BOOLEAN DEFAULT TRUE,
                            b_stats   IN BOOLEAN DEFAULT TRUE) IS
    c_cmdname       CONSTANT VARCHAR2(15) := 'SpatialIndexAll';
    c_cmdtype       CONSTANT VARCHAR2(6)  := 'TUNING';
    c_appliesto     CONSTANT VARCHAR2(20) := 'All Feature Classes';
    --
    c_feedback      CONSTANT VARCHAR2(43) := 'Possible orphan in ALL_SDO_GEOM_METADATA: ';
    c_feedback1     CONSTANT VARCHAR2(40) := 'Run EXEC GOOM.SetMBRAll; for: ';
    v_feature       GetTableGeomsFromAllMBR%ROWTYPE;
    v_geometry      COLS.COLUMN_NAME%TYPE;
    v_ownertable    VARCHAR2(61);
    i_count         PLS_INTEGER;
    --
  BEGIN
    ProcessStart( c_cmdname, v_schema);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_msgStart|| v_schema);
    -- Check that Oracle metadata does exist
    SELECT COUNT(*) INTO i_count FROM ALL_SDO_GEOM_METADATA WHERE OWNER = v_schema;
    IF i_count  < 1 THEN
     RAISE e_NoMetadataFound; -- No Oracle metadata found.
    END IF;
    FOR v_feature IN GetTableGeomsFromAllMBR( v_schema ) LOOP
      v_ownertable:= v_schema ||'.'|| v_feature.table_name;
      v_geometry  := v_feature.column_name;
      IF NOT chkView( v_ownertable ) THEN
        RTree( v_ownertable, v_geometry, b_geomopt, b_stats);
      END IF;
    END LOOP;
    ProcessComplete( c_cmdname, v_schema);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_msgComplete||v_schema);
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgWarning, c_msgTableNotFound|| v_ownertable);
      Response( c_cmdname || c_msgVerify, c_feedback || v_ownertable ||'.'|| v_geometry);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geometry, c_msgGoomInternal);
      ProcessTerminate( c_cmdname, v_schema);
    WHEN e_NoMetadataFound THEN
      Response( c_cmdname || c_msgError, c_msgNoMetadataFound|| v_schema);
      Response( c_cmdname || c_msgSolution, c_feedback1 || v_schema);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_schema, c_msgGoomInternal);
      ProcessTerminate( c_cmdname, v_schema);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geometry, c_feedback || v_schema, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geometry, c_msgOraError || SQLCODE);
  END SpatialIndexAll;
  --------------------------------------------------------------------------------------
  -- Stats calculates the statistics for the objects in the current (or specified) schema.
  -- Syntax: EXEC GOOM.STATS([v_schema]);
  --         v_schema : optional and can be used by a DBA to operate on any schema.
  --
  PROCEDURE Stats ( v_schema IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(5)  := 'Stats';
    c_cmdtype       CONSTANT VARCHAR2(6)  := 'TUNING';
    c_appliesto     CONSTANT VARCHAR2(20) := 'Current schema.';
    c_feedback      CONSTANT VARCHAR2(43) := 'Stats collected for all objects in: ';
    --
  BEGIN
    DBMS_STATS.GATHER_SCHEMA_STATS( v_schema, CASCADE=>TRUE );
    Response( c_cmdname, c_feedback || v_schema);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_feedback || v_schema);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, c_appliesto, v_schema, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_appliesto, c_msgOraError || SQLCODE);
  END Stats;
  --------------------------------------------------------------------------------------
  -- DelStats deletes the database statistics associated with the current (or specified) schema.
  -- Syntax:  EXEC GOOM.DelSTATS([v_schema]);
  --         v_schema : optional and can be used by a DBA to operate on any schema.
  --
  PROCEDURE DELStats ( v_schema IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(8)  := 'DelStats';
    c_cmdtype       CONSTANT VARCHAR2(6)  := 'TUNING';
    c_appliesto     CONSTANT VARCHAR2(20) := 'Current schema.';
    c_feedback      CONSTANT VARCHAR2(42) := 'Stats removed for all objects in: ';
  BEGIN
    DBMS_STATS.DELETE_SCHEMA_STATS( v_schema );
    Response( c_cmdname, c_feedback || v_schema);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_feedback || v_schema);
    EXCEPTION
      WHEN OTHERS THEN
        REPORT_ERROR ( c_cmdname, c_appliesto, v_schema, SQLCODE, SQLERRM);
        WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_appliesto, c_msgOraError || SQLCODE);
  END DELStats;
  --------------------------------------------------------------------------------------
  -- Autotune is a multi-function that will tune a schema that has been loaded using
  -- GeoMedia's Export to Oracle Object Model data.  The process repairs NULL geometries,
  -- sets default Oracle metadata (MBR), create spatial indexes, and generates statistics.
  -- Syntax: EXEC GOOM.Autotune([v_schema]);
  --         v_schema : optional and can be used by a DBA to operate on any schema.
  --
  PROCEDURE AutoTune( v_schema IN VARCHAR2 DEFAULT USER) IS
    c_cmdname   CONSTANT VARCHAR2(9) := 'AutoTune';
    c_cmdtype   CONSTANT VARCHAR2(7) := 'TUNING';
    c_appliesto CONSTANT VARCHAR2(20):= 'All Feature Classes';
    --
    c_stage1    VARCHAR2(36):='Stage 1: Null Geometry Correction...';
    c_stage2    VARCHAR2(57):='Stage 2: Setting default extents in Oracle''s metadata...';
    c_stage3    VARCHAR2(36):='Stage 3: Creating spatial indexes...';
    c_stage4    VARCHAR2(36):='Stage 4: Generating Statistics...';
    c_msgpad    PLS_INTEGER :=28;
  BEGIN
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_msgStart || v_schema);
    Response( c_cmdname, c_msgStart || v_schema, c_msgpad);
    DotLine;
    Response( c_cmdname, c_stage1, c_msgpad);
    DelSidx ( v_schema );
    FixNullGeoms ( v_schema );
    Response( c_cmdname,  c_stage2, c_msgpad);
    SetMBRAll( v_schema );
    Response( c_cmdname, c_stage3, c_msgpad);    
    SpatialIndexAll( v_schema );
    Response( c_cmdname, c_stage4, c_msgpad);
    Stats( v_schema );
    DashLine;
    Response( c_cmdname, c_msgComplete || v_schema, c_msgpad);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_msgComplete || v_schema);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, c_appliesto, v_schema, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_appliesto, c_msgOraError || SQLCODE);
  END AutoTune;

  -- -----------------------------------------------------------------------------------------------------------------
  -- GEOMEDIA's GDOSYS METADATA PROCEDURES: Working with GDOSYS
  -- -----------------------------------------------------------------------------------------------------------------
  
  -- SetGDOSYSMetadata sets default entries in GDOSYS for the specified table/view.  This
  -- creates the feature class metadata used by GeoMedia.  It is the command line equivalent of
  -- running Database Utilities and just accepting the defaults.  Only tables with a single 
  -- primary key are supported.  A DBA user must use owner.table for the tablename.
  -- Syntax: EXEC GOOM.SetGDOSYSMetadata(v_tablename,[v_seqname],[v_cs],[v_geomType],[v_keycolumn],[v_num38],[v_num10]);
  --         v_tablename : the name of the table or view.  It is required.
  --         v_seqname (optional) : the name of the sequence that populates the primary key.  If
  --                                it does not exist, it will be created.
  --         v_cs (optional, default D) : the csguid of the coordinate system to assign.  Use 'D' to use
  --                                      the default coordinate system assigned to the schema.
  --         v_geomType (optional) : the GTYPE to use for empty geometries.  Otherwise the GTYPE will 
  --                                 be picked up automatically by sampling the geometry gtype.
  --         v_keycolumn (optional): used only if v_tablename : actually a view, then it
  --                                 must be the key preserved column used in the view.
  --         v_num38 (optional) : an override for NUMBER(38) PKEY assignments.  Any value here
  --                              except 'LONG' forces override.  Use only if you need interoperability 
  --                              with ESRI NUMBER(38).
  --         v_num10 (optional) : an override for all NUMBER(10) assignments.  Forces LONG.
  --                              Any value here except 'DOUBLE' forces override.  Use only if you need 
  --                              interoperability with GTECH NUMBER(10) for G3E columns.
  --   
  PROCEDURE SetGDOSYSMetadata( v_tablename   IN VARCHAR2,
                               v_seq_name    IN VARCHAR2 DEFAULT NULL,
                               v_cs          IN VARCHAR2 DEFAULT 'DEFAULT',
                               i_geomtype_in IN INTEGER  DEFAULT NULL,
                               v_keycolumn   IN VARCHAR2 DEFAULT NULL,
                               v_num38       IN VARCHAR2 DEFAULT 'LONG',
                               v_num10       IN VARCHAR2 DEFAULT 'DOUBLE') IS
    --
    c_cmdname           CONSTANT VARCHAR2(17) := 'SetGDOSYSMetadata';
    c_cmdtype           CONSTANT VARCHAR2(8)  := 'GDOSYS';
    c_feedback          CONSTANT VARCHAR2(31) := 'Inserted GDOSYS metadata for: ' ;
    -- Initialized Variables
    v_table_data         COLS%ROWTYPE;
    v_csguid             VARCHAR2(64) := NULL;      
    v_column             VARCHAR2(32) := NULL;
    v_pkey               VARCHAR2(32) := NULL;
    v_geometry           VARCHAR2(32) := NULL;
    v_boolean            VARCHAR2(32) := NULL;
    i_col_id             INTEGER      := 0;
    i_inum               INTEGER      := 1;
    i_geomtype           INTEGER      :=-1;
    i_pgeomcol           INTEGER      := 0;
    i_datatype           INTEGER      := 0;
    i_datasubtype        INTEGER      := 0;
    i_precision          INTEGER      := 0;
    i_scale              INTEGER      := 0;
    i_charlength         INTEGER      := 0;
    i_isview             PLS_INTEGER  := 0;
    i_fieldtype          INTEGER      := 0;
    i_keyfield           INTEGER      := 0;
    i_displayable        INTEGER      :=-1;
    i_cscount            PLS_INTEGER  := 0;
    v_fieldDesc          VARCHAR2(255):= 'From Oracle';
    i_num                INTEGER      := 1;
    v_defcs              VARCHAR2(38) := '{AB111111-1111-1111-1111-111111111111}';
    i_n10ov              PLS_INTEGER  :=0;
    -- Non initialized Variables
    v_ipad               VARCHAR2(3);
    v_sql                VARCHAR2(1024);
    v_debug              VARCHAR2(255);
    i_desc_chk           PLS_INTEGER;
    v_table_comment      VARCHAR2(255);
    v_col_comment        VARCHAR2(255);
    v_sequence           VARCHAR2(61);
    v_seqName            VARCHAR2(30);
    v_seqOwner           VARCHAR2(30);
    v_geometry_field     MDSYS.SDO_GEOMETRY;
    v_fieldformat        VARCHAR2(255);
    v_viewindex          VARCHAR2(64);
    v_dtype              VARCHAR2(32);
    v_gtype              VARCHAR2(4);
    v_viewname           VARCHAR2(32);
    -- added to support owner.table
    v_ownertable         VARCHAR2(61);
    v_schema              VARCHAR2(30);
    v_table              VARCHAR2(30);
    --
  BEGIN
    v_ownertable := GetOwnerObject( v_tablename);
    v_schema      := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      := SplitOwnerObject( v_ownertable, 'TABLE'); 
    -- Check Inputs
    IF NOT ChkTable( v_ownertable ) AND NOT ChkView( v_ownertable ) AND NOT ChkMView( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    -- Delete any existing metadata
    v_debug := 'Checking Metadata Existence';
    IF chkMetadata( v_ownertable ) = TRUE THEN
      -- Does any metadata exist for this table/view?
      DelGDOSYSMetadata( v_ownertable ); -- Delete existing metadata.
      v_debug := 'Completed Metadata Deletion';
    END IF;
    -- Use Default CSGUID if one is available.
    v_debug := 'Start CS Assignment - Auto';
    IF UPPER( v_cs ) = 'D' OR UPPER( v_cs ) = 'DEFAULT' THEN
      -- User indicates default CS
      v_sql:='SELECT COUNT(1) FROM GDOSYS.GPARAMETERS WHERE GPARAMETER LIKE '''|| v_schema ||'.DefaultCoordinateSystem''';
      EXECUTE IMMEDIATE v_sql INTO i_cscount;
      IF i_cscount > 0 THEN
        -- Default exists so use it
        v_sql:='SELECT gvalue FROM GDOSYS.GPARAMETERS WHERE GPARAMETER LIKE '''|| v_schema ||'.DefaultCoordinateSystem''';
        EXECUTE IMMEDIATE v_sql INTO v_csguid;
      ELSE
        -- Default does not exists, use a dummy default.
        v_csguid := v_defcs; -- Assign dummy cs and warn user.
        Response( c_cmdname || c_msgWarning,'Default CS not found! Assign correct CS via Database Utilities');
        WRITE_RESULTS( v_schema, c_msgWarning, c_cmdname, v_ownertable, 'Incorrect Coordinate System Assigned - Assign new CS via DBUTILS');
      END IF; 
    ELSE
      v_debug := 'Start CS Assignment - Manual';
      --EXECUTE IMMEDIATE 'SELECT LENGTH(:vcs) FROM DUAL' INTO i_cscount USING v_cs;
      i_cscount := LENGTH( v_cs );
      IF i_cscount = 38 THEN
        -- User entered a CSGUID.
        v_csguid := v_cs;
        v_debug := 'Manual CS Entry checked';
      ELSE
        v_csguid := v_defcs; -- Assign dummy cs and warn user.
        Response( c_cmdname || c_msgWarning,'Specified CS not found! Assign correct CS via Database Utilities');
        WRITE_RESULTS( v_schema, c_msgWarning, c_cmdname, v_ownertable, 'Incorrect Coordinate System Assigned - Assign new CS via DBUTILS');
      END IF;
    END IF;
    -- End Default CS section
    v_debug := 'Completed Default CS Assignment';
    -- Collect information about the columns in the table.
    FOR v_table_data IN (SELECT COLUMN_NAME, DATA_TYPE, DATA_PRECISION, DATA_SCALE, CHAR_COL_DECL_LENGTH FROM ALL_TAB_COLUMNS WHERE TABLE_NAME = v_table and OWNER = v_schema) LOOP
      v_column     := v_table_data.COLUMN_NAME;
      v_dtype      := v_table_data.DATA_TYPE;
      i_precision  := v_table_data.DATA_PRECISION;
      i_scale      := v_table_data.DATA_SCALE;
      i_charlength := v_table_data.CHAR_COL_DECL_LENGTH;
      v_sql        := 'SELECT SUBSTR(NVL(COMMENTS,''None''),1,255)
                         FROM ALL_COL_COMMENTS
                        WHERE OWNER=:vowner AND TABLE_NAME=:vtablename AND COLUMN_NAME=:vcolname';
      EXECUTE IMMEDIATE v_sql INTO v_col_comment USING v_schema, v_table, v_column;
      v_debug := 'Collected Table Information';
      -- Information collected.  Now start making metadata assignments.
      -- Primary Key information is first. Handle differently for views.
      IF chkPkey( v_ownertable, v_column) OR v_column = v_keycolumn THEN
        -- Check whether the column is a key column
        i_keyfield := -1;
        v_pkey     := v_column;
        v_debug    := 'Key Column: ' || v_column;
      ELSE
        i_keyfield := 0;
        v_debug    := 'Not Key Column: ' || v_column;
      END IF;
      IF v_keycolumn IS NOT NULL AND i_isview = 0 THEN
        -- Check whether this is a view and handle if it is.
        i_isview    := 1;
        v_viewindex := SUBSTR( v_table, 1, 22 ) || '_GMINDX1';
        WHILE chkViewIndex( v_schema ||'.'|| v_viewindex) = TRUE -- If index exists...
         LOOP
          -- Loop until a unique name is created.
          i_inum      := i_inum + 1; -- Increment counter.
          v_ipad      := TO_CHAR( i_inum ); -- Convert counter to character.
          v_viewindex := SUBSTR( v_table, 1, 22 ) || '_GMINDX' || v_ipad;
        END LOOP;
        v_viewname := v_table;
        v_sql      := 'SELECT COLUMN_ID FROM ALL_TAB_COLUMNS WHERE OWNER='''|| v_schema ||''' AND TABLE_NAME=''' || v_viewname || ''' AND COLUMN_NAME=''' || v_keycolumn || '''';
        EXECUTE IMMEDIATE v_sql INTO i_col_id;
      END IF;
      v_debug := 'Completed Keyfield Assignment';
      -- Create the INDEXID for each column in the table
      v_sql := 'INSERT INTO GDOSYS.FIELDLOOKUP(INDEXID, FEATURENAME, FIELDNAME) 
                SELECT GDOSYS.fieldlookupindexid1.nextval, '''|| v_ownertable ||''','''|| v_column ||''' FROM DUAL';
      EXECUTE IMMEDIATE (v_sql);
      COMMIT;
      -- Set parameters for AttributeProperties, GeometryProperties, GFieldMapping and GFeatures.
      CASE v_dtype
        WHEN 'VARCHAR2' THEN
          -- Character column metadata
          i_displayable := -1;
          v_fieldformat := NULL;
          i_scale       := NULL;
          IF i_charlength > 255 THEN
            i_fieldtype := 12; --  MEMO
          ELSE
            i_fieldtype := 10; --  TEXT
          END IF;
          IF v_col_comment = 'None' THEN
            v_fieldDesc := 'VARCHAR2(' || i_charlength || ')';
          ELSE
            v_fieldDesc := v_col_comment;
          END IF;
        WHEN 'CHAR' THEN
          -- Character column metadata
          i_displayable := -1;
          v_fieldformat := NULL;
          i_scale       := NULL;
          IF i_charlength > 255 THEN
            i_fieldtype := 12; --  MEMO
          ELSE
            i_fieldtype := 10; --  TEXT
          END IF;
          IF v_col_comment = 'None' THEN
            v_fieldDesc := 'CHAR(' || i_charlength || ')';
          ELSE
            v_fieldDesc := v_col_comment;
          END IF;
        WHEN 'CLOB' THEN
          -- Character Long Object column metadata
          i_displayable := -1;
          v_fieldformat := NULL;
          i_scale       := NULL;
          i_fieldtype   := 12; --  MEMO
          IF v_col_comment = 'None' THEN
            v_fieldDesc := 'CLOB';
          ELSE
            v_fieldDesc := v_col_comment;
          END IF;
        WHEN 'NUMBER' THEN
          -- NUMBER column metadata
          i_displayable := -1;
          v_fieldformat := 'General Number';
          IF i_precision = 1 AND (i_scale = 0 OR i_scale IS NULL) THEN
            i_fieldtype   := 1; -- BOOLEAN
            v_fieldformat := 'True/False';
            v_boolean     := v_column;
            IF v_col_comment = 'None' THEN
              v_fieldDesc := 'NUMBER(' || i_precision || ',' || i_scale || ')';
            ELSE
              v_fieldDesc := v_col_comment;
            END IF;
          ELSIF i_precision > 1 AND i_precision < 10 AND i_scale = 0 THEN
            i_fieldtype := 4; -- LONG
            IF v_col_comment = 'None' THEN
              v_fieldDesc := 'NUMBER(' || i_precision || ',' || i_scale || ')';
            ELSE
              v_fieldDesc := v_col_comment;
            END IF;
          ELSIF i_precision IS NULL AND i_scale = 0 THEN
            i_fieldtype := 4; -- LONG, this is a special type of NUMBER(38).
            IF v_col_comment = 'None' THEN
              v_fieldDesc := 'NUMBER(38) - Oracle INTEGER';
            ELSE
              v_fieldDesc := v_col_comment;
            END IF;
          ELSE
            i_fieldtype := 7; -- DOUBLE
            IF v_col_comment = 'None' THEN
              v_fieldDesc := 'NUMBER(' || i_precision || ',' || i_scale || ')';
            ELSE
              v_fieldDesc := v_col_comment;
            END IF;
          END IF;
          -- NUMBER(38) Override
          -- If the Key field is NUMBER(38) and has a sequence, it still must be long.
          -- This has ramifications for interop so there is an override.
          -- The latest version of 5.2 and 6.0 solve this.  The override is not
          -- necessary.  Commenting out section for the time being.
          IF i_fieldtype = 7 AND i_keyfield = -1 AND v_seq_name IS NOT NULL THEN
            IF v_num38 = 'LONG' OR v_num38 IS NULL THEN
              i_fieldtype := 4; -- this is the default and the proper assignment
            ELSE
              i_fieldtype := 7; -- this is the interop override
            END IF;
          END IF; -- End NUMBER(38) override section
          -- NUMBER(10) Override
          -- By default NUMBER(10) is set to default.  If v_num10 is set to anything but DOUBLE
          -- NUMBER(10) will be overridden to LONG.  This is for applications that require it.
          -- Normally this is not used.
          IF i_precision = 10 AND ( i_scale = 0 OR i_scale IS NULL) AND v_seq_name IS NULL and v_num10 <> 'DOUBLE' THEN
            i_fieldtype := 4;
            IF i_keyfield = -1 THEN
              i_n10ov :=0; -- pkey fields are always written to GFIELDMAPPING so skip.
            ELSE
              i_n10ov :=1; -- not a pkey field so write the override to GFIELDMAPPING.
            END IF;
          ELSE
            i_n10ov   :=0;   -- No override required.
          END IF; -- End Number(10) override section
          --
        WHEN 'FLOAT' THEN
          -- FLOATING column metadata
          i_displayable := -1;
          v_fieldformat := 'General Number';
          i_fieldtype   := 7; -- Double
          i_scale       := 6;
          IF v_col_comment = 'None' THEN
            v_fieldDesc := 'FLOAT';
          ELSE
            v_fieldDesc := v_col_comment;
          END IF;
        WHEN 'INTEGER' THEN
          -- INTEGER column metadata
          i_displayable := -1;
          v_fieldformat := 'General Number';
          i_fieldtype   := 4; -- LONG
          i_scale       := 0;
          IF v_col_comment = 'None' THEN
            v_fieldDesc := 'INTEGER';
          ELSE
            v_fieldDesc := v_col_comment;
          END IF;
        WHEN 'DATE' THEN
          -- DATE column metadata
          i_displayable := -1;
          v_fieldformat := 'Date/Time';  -- Changed to match DBUtils
          i_fieldtype   := 8; -- Date
          i_scale       := 6;
          IF v_col_comment = 'None' THEN
            v_fieldDesc := 'DATE';
          ELSE
            v_fieldDesc := v_col_comment;
          END IF;
        WHEN 'SDO_GEOMETRY' THEN
          -- Set parameters for geometry types
          i_displayable := -1;
          i_fieldtype   := NULL;
          IF i_geomtype_in = 5 THEN
            i_datatype  := 33;
          ELSE
            i_datatype  := 32;
          END IF;
          v_fieldformat := NULL;
          i_scale       := NULL;
          IF i_geomtype_in IS NULL THEN
            -- Extract the first GTYPE.  This will determine the geometry type.
            -- If there is more than one GTYPE, result is spatialAny
            v_gtype := GetGTYPE(v_ownertable, v_column);
            CASE i_num
              WHEN 1 THEN
                i_pgeomcol := -1;
              ELSE
                i_pgeomcol := 0;
            END CASE;
            CASE SUBSTR(v_gtype, 4) -- Extract the 4th digit of the GTYPE as the geometry type.
              WHEN 1 THEN
                -- This is an Oracle Point type.
                -- Check for Native Point or GeoMedia text
                v_sql := 'select ' || v_column || ' from ' || v_ownertable || ' where rownum = 1';
                EXECUTE IMMEDIATE (v_sql) INTO v_geometry_field;
                IF v_geometry_field.sdo_point.X IS NULL THEN
                  -- Not a Native Point
                  IF v_geometry_field.sdo_elem_info(3) = 6001 AND v_geometry_field.sdo_elem_info(2) = 0 THEN
                    i_geomtype := 5; -- Text
                    IF v_col_comment = 'None' THEN
                      v_fieldDesc := 'Geomedia Text Geometry';
                    ELSE
                      v_fieldDesc := v_col_comment;
                    END IF;
                  ELSE
                    -- Handle as a GeoMedia point or and Oracle Oriented point
                    i_geomtype := 10; -- Point
                    IF v_col_comment = 'None' THEN
                      v_fieldDesc := 'Oracle Point Geometry';
                    ELSE
                      v_fieldDesc := v_col_comment;
                    END IF;
                  END IF;
                ELSE
                  -- Handle as a Native Point
                  i_geomtype := 10; -- Point
                  IF v_col_comment = 'None' THEN
                    v_fieldDesc := 'Oracle Native Point Geometry';
                  ELSE
                    v_fieldDesc := v_col_comment;
                  END IF;
                END IF;
                --
              WHEN 2 THEN
                i_geomtype := 1; -- Line
                IF v_col_comment = 'None' THEN
                  v_fieldDesc := 'Oracle Linear Geometry';
                ELSE
                  v_fieldDesc := v_col_comment;
                END IF;
              WHEN 3 THEN
                -- Check for coverage
                v_sql := 'select ' || v_column || ' from ' || v_ownertable || ' where rownum = 1';
                EXECUTE IMMEDIATE (v_sql)
                  INTO v_geometry_field;
                IF v_geometry_field.sdo_elem_info(3) = 6002 AND v_geometry_field.sdo_elem_info(2) = 0 THEN
                  i_geomtype := 4; -- coverage
                  IF v_col_comment = 'None' THEN
                    v_fieldDesc := 'GeoMedia Coverage Geometry';
                  ELSE
                    v_fieldDesc := v_col_comment;
                  END IF;
                ELSE
                  i_geomtype := 2; -- Area
                  IF v_col_comment = 'None' THEN
                    v_fieldDesc := 'Oracle Polygon Geometry';
                  ELSE
                    v_fieldDesc := v_col_comment;
                  END IF;
                END IF;
              WHEN 4 THEN
                i_geomtype := 3; -- Compound
                IF v_col_comment = 'None' THEN
                  v_fieldDesc := 'Oracle Collection Geometry';
                ELSE
                  v_fieldDesc := v_col_comment;
                END IF;
              WHEN 5 THEN
                i_geomtype := 3; -- Compound
                IF v_col_comment = 'None' THEN
                  v_fieldDesc := 'Oracle Multipoint Geometry';
                ELSE
                  v_fieldDesc := v_col_comment;
                END IF;
              WHEN 6 THEN
                i_geomtype := 3; -- Compound
                IF v_col_comment = 'None' THEN
                  v_fieldDesc := 'Oracle Multiline Geometry';
                ELSE
                  v_fieldDesc := v_col_comment;
                END IF;
              WHEN 7 THEN
                i_geomtype := 3; -- Compound
                IF v_col_comment = 'None' THEN
                  v_fieldDesc := 'Oracle Multipolygon Geometry';
                ELSE
                  v_fieldDesc := v_col_comment;
                END IF;
              ELSE
                i_geomtype := 3; --Probably empty, set to compound.
                IF v_col_comment = 'None' THEN
                  v_fieldDesc := 'Oracle Unknown Geometry';
                ELSE
                  v_fieldDesc := v_col_comment;
                END IF;
            END CASE;
          --
          ELSE
            --  Use the Geom Type passed by user
            i_geomtype := i_geomtype_in;
            -- Added 7/23/2007 at request of Gary Whitt
	        i_pgeomcol := -1; -- Set PRIMARYGEOMETRYFLAG if geometry type passed in
            case i_geomtype -- Set FIELDDESCRIPTION if geometry type passed in
              when 1 then -- line
                v_fieldDesc := 'Oracle Linear Geometry';
              when 2 then -- area
                v_fieldDesc := 'Oracle Polygon Geometry';
              when 3 then -- compound
                v_fieldDesc := 'Oracle Collection Geometry';
              when 4 then -- coverage
                v_fieldDesc := 'GeoMedia Coverage Geometry';
              when 5 then -- text
                v_fieldDesc := 'Geomedia Text Geometry';
              when 10 then -- point
                v_fieldDesc := 'Oracle Point Geometry';
              else
                v_fieldDesc := 'Oracle Unknown Geometry';
            end case;
            -- End Addition.
           END IF;
          IF i_geomtype_in = 5 THEN
            i_datasubtype := i_geomtype_in;
          ELSE
            i_datasubtype := i_geomtype;
          END IF;
          v_geometry := v_column;
          i_num      := i_num + 1;
      WHEN 'SDO_GEORASTER' THEN
        i_geomtype    := 4;
        i_datasubtype :=i_geomtype;
        i_displayable := -1;
        i_fieldtype   := NULL;
        i_pgeomcol    := -1;
        v_geometry    := v_column;
        IF v_col_comment = 'None' THEN
          v_fieldDesc := 'Raster Image Storage';
        ELSE
          v_fieldDesc := v_col_comment;
        END IF;
      ELSE
          -- Nothing matches, field is not supported.
          i_displayable := 0;
          v_fieldformat := NULL;
          i_precision   := NULL;
          i_scale       := NULL;
          i_fieldtype   := NULL;
          IF v_col_comment = 'None' THEN
            v_fieldDesc := 'Unsupported Column Type';
          ELSE
            v_fieldDesc := v_col_comment;
          END IF;
       END CASE; -- End column assignments.
      v_debug := 'Completed Parameter Assignment:'|| i_keyfield;
      -- Insert Key Field Information into GFIELDMAPPING
      IF i_keyfield = -1 THEN
        IF v_seq_name ='AUTO' THEN  -- Automatically generate sequence.
          v_debug    := 'AUTO Sequence';
          v_sequence := GetSequenceName(v_ownertable,v_pkey);  -- Get standard sequence name
          IF NOT chkSequence(v_sequence) THEN                  -- If the sequence does not exist...
            v_debug    := 'Generating Sequence';
            CreateNewSequence( v_ownertable, v_pkey, v_sequence); -- Create one.
            v_seqName  := SplitOwnerObject( v_sequence, 'OBJECT');
            v_seqOwner := SplitOwnerObject( v_sequence, 'OWNER');
          END IF;
        ELSIF v_seq_name IS NULL THEN
          v_seqName  := NULL;
          v_seqOwner := NULL;
        ELSE   -- Get sequence name and owner information from input.
          v_debug    := 'Using: '|| v_seq_name;
          v_seqName  := SplitOwnerObject( v_seq_name, 'OBJECT');
          v_seqOwner := SplitOwnerObject( v_seq_name, 'OWNER');
        END IF;
        -- Primary key
        v_sql := 'insert into GDOSYS.GFIELDMAPPING SELECT '''|| v_schema ||''','''|| v_table ||''','''|| v_pkey||''','''|| i_fieldtype||
                 ''',NULL,NULL,'''|| v_seqOwner ||''','''|| v_seqName ||''' from dual';
        EXECUTE IMMEDIATE ( v_sql );
        v_debug := 'Inserting GfieldMapping 1';
      END IF;
      v_debug := 'Completed automated seq generation for key field insert';
      IF i_fieldtype = 1 THEN
        -- Boolean
        v_sql := 'insert into GDOSYS.GFIELDMAPPING SELECT '''|| v_schema ||''','''|| v_table ||''',''' || v_boolean ||''','''|| i_fieldtype ||
                 ''',NULL,NULL,NULL,NULL from dual';
        EXECUTE IMMEDIATE (v_sql);
        v_debug := 'Inserting GfieldMapping 2';
      END IF;
      v_debug := 'Completed standard keyfield insert';
      -- Insert any overrides
      -- Insert Geometry Information in GEOMETRYPROPERTIES
      CASE v_dtype
        WHEN 'SDO_GEOMETRY' THEN
          v_sql := 'insert into GDOSYS.GEOMETRYPROPERTIES(PRIMARYGEOMETRYFLAG, GEOMETRYTYPE, GCOORDSYSTEMGUID, FIELDDESCRIPTION, INDEXID) select ''' ||
                   i_pgeomcol || ''', ''' || i_geomtype || ''', ''' || v_csguid || ''', ''' || v_fieldDesc ||
                   ''', INDEXID from GDOSYS.FIELDLOOKUP WHERE FEATURENAME = '''|| v_ownertable || ''' and FIELDNAME = ''' || v_geometry || '''';
          EXECUTE IMMEDIATE ( v_sql );
          -- special text handling
          IF i_datasubtype = 5 THEN
            i_datatype := 33;
          END IF;
          v_sql := 'insert into GDOSYS.GFIELDMAPPING SELECT '''|| v_schema ||''',''' || v_table || ''',''' || v_geometry || ''',''' || i_datatype || ''',''' ||
                   i_datasubtype || ''',''' || v_csguid || ''',NULL,NULL from dual';
          EXECUTE IMMEDIATE (v_sql);
        WHEN 'SDO_GEORASTER' THEN
          v_sql := 'insert into GDOSYS.GEOMETRYPROPERTIES(PRIMARYGEOMETRYFLAG, GEOMETRYTYPE, GCOORDSYSTEMGUID, FIELDDESCRIPTION, INDEXID) select ''' ||
                   i_pgeomcol || ''', ''' || i_geomtype || ''', ''' || v_csguid || ''', ''' || v_fieldDesc ||
                   ''', INDEXID FROM GDOSYS.FIELDLOOKUP WHERE FEATURENAME = '''|| v_ownertable || ''' and FIELDNAME = ''' || v_geometry || '''';
          EXECUTE IMMEDIATE ( v_sql );
          IF i_datasubtype = 4 THEN
            i_datatype := 32;
          END IF;
          v_sql := 'insert into GDOSYS.GFIELDMAPPING SELECT '''|| v_schema ||''',''' || v_table || ''',''' || v_geometry || ''',''' || i_datatype || ''',''' ||
                   i_datasubtype || ''',''' || v_csguid || ''',NULL,NULL from dual';
          EXECUTE IMMEDIATE ( v_sql );
        ELSE
          v_sql := 'insert into GDOSYS.ATTRIBUTEPROPERTIES(ISKEYFIELD, FIELDDESCRIPTION, FIELDFORMAT, FIELDTYPE, ISFIELDDISPLAYABLE, FIELDPRECISION, INDEXID) SELECT ''' ||
                   i_keyfield || ''', ''' || v_fieldDesc || ''', ''' || v_fieldformat || ''', ''' || i_fieldtype || ''', ''' || i_displayable ||''', ''' ||
                   i_scale || ''', INDEXID from GDOSYS.FIELDLOOKUP where FEATURENAME = '''|| v_ownertable ||''' and FIELDNAME = ''' || v_column || '''';
          EXECUTE IMMEDIATE ( v_sql );
          -- Insert an override for NUMBER(10) if required.
          IF i_n10ov = 1 THEN
             v_sql := 'INSERT INTO GDOSYS.GFIELDMAPPING SELECT '''|| v_schema ||''',''' || v_table || ''',''' || v_column || ''',''' || i_fieldtype ||''',NULL,NULL,NULL,NULL from dual';
             EXECUTE IMMEDIATE ( v_sql );
          END IF;
          -- End Number(10) override.
      END CASE;
      v_debug := 'Completed geometry insert';
    END LOOP;
    -- Insert view information in GINDEX_COLUMNS:
    IF i_isview = 1 THEN
      v_sql := 'INSERT INTO GDOSYS.GINDEX_COLUMNS ( OWNER,OBJECT_NAME,INDEX_NAME, INDEX_TYPE, COLUMN_NAME, COLUMN_POSITION) select '''
                || v_schema ||''',''' || v_viewname || ''', ''' || v_viewindex || ''',''P'',''' || v_keycolumn || ''', ' || i_col_id || ' from dual';
      EXECUTE IMMEDIATE ( v_sql );
      COMMIT;
      -- Need to assign keyfield in attribute properties.  This has not been done yet.
      v_debug := 'Completed GINDEX_COLUMNS insert';
    END IF;
    -- Insert feature class into GFEATURES.  This is the last step for the table.
    IF i_geomtype = 5 THEN
      i_geomtype := 33;
    END IF;
    -- Check for existing database comments.
    v_sql := 'select count(1) FROM ALL_TAB_COMMENTS WHERE OWNER = '''|| v_schema ||''' AND TABLE_NAME =''' || v_table || '''';
    EXECUTE IMMEDIATE ( v_sql ) INTO i_desc_chk;
    IF i_desc_chk > 0 THEN
      v_sql := 'select SUBSTR(NVL(COMMENTS,''None''),1,255) FROM ALL_TAB_COMMENTS WHERE OWNER = '''|| v_schema ||''' AND TABLE_NAME =''' || v_table || '''';
      EXECUTE IMMEDIATE ( v_sql ) INTO v_table_comment;
    ELSE
      v_table_comment := NULL;
    END IF;
    v_sql := 'insert into GDOSYS.GFEATURES(FEATURENAME,GEOMETRYTYPE,PRIMARYGEOMETRYFIELDNAME,FEATUREDESCRIPTION) select '''|| v_schema ||'.'||
             v_table || ''', ''' || i_geomtype || ''', ''' || v_geometry || ''', ''' || v_table_comment || ''' from dual';
    EXECUTE IMMEDIATE (v_sql);
    COMMIT;
    v_debug := 'Completed gfeatures insert';
    Response( c_cmdname, c_feedback || v_ownertable);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable, c_feedback);
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname  || c_msgError, c_msgTableNotFound|| v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound|| v_ownertable, c_msgGoomInternal);
    WHEN OTHERS THEN
      ROLLBACK;
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_column, v_debug, v_sql, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_column, c_msgOraError || SQLCODE);
      --
  END SetGDOSYSMetadata;
  --------------------------------------------------------------------------------------
  -- SetGDOSYSMetadataAlL sets default entries in GDOSYS for all tables/views in your schema.
  -- Syntax: EXEC GOOM.SetGDOSYSMetadataAll; or EXEC GOOM.SetGDOSYSMetadataAll([v_schema]);
  --         Set the USER constants prior to compiling to control how the procedure works.
  --         v_schema : optional and can be used by a DBA to operate on any schema.    
  --    
  PROCEDURE SetGDOSYSMetadataAll ( v_schema IN VARCHAR2 DEFAULT USER) IS
  c_cmdname   CONSTANT VARCHAR2(20) :='SetGDOSYSMetadataAll';
  -- User parameters
  v_seq       VARCHAR2(30) :='AUTO';  -- Set to 'AUTO' to auto assign or generate new sequences.
  v_cs        VARCHAR2(38) :='D';     -- Use default CS (D), or enter CSGUID to assign.
  v_num38     VARCHAR2(32) :='LONG';  -- Set to 'LONG' for normal operations. Anything else will set to override NUMBER(38)pkey to DOUBLE
  v_num10     VARCHAR2(8)  :='DOUBLE';-- Set to 'DOUBLE' for normal operations. Anything else will override NUMBER(10) to LONG
  -- End User Parameters
  v_feature   GetTableNames%ROWTYPE;
  BEGIN
    RESPONSE( c_cmdname, c_msgStart || v_schema,28);
    FOR v_feature in GetTableNames ( v_schema ) LOOP
      SetGDOSYSMetadata( v_feature.table_name, v_seq, v_cs,NULL,NULL, v_num38, v_num10);
    END LOOP;
    Response( c_cmdname, c_msgComplete || v_schema,28);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, v_feature.table_name, v_schema,SQLCODE,SQLERRM);
  END SetGDOSYSMetadataAll;
  --------------------------------------------------------------------------------------
  -- SetDefaultCS sets the default coordinate system used by the specified schema. Setting a
  -- NULL for v_csguid disables the default. The CSGUID comes from the GDOSYS.GCOORDSYSTEM table.
  -- Syntax: EXEC GOOM.SetDefaultCS(c_csguid, [v_schema]);
  --         v_schema : optional and can be used by a DBA to operate on any schema.    
  --
  PROCEDURE SetDefaultCS( v_csguid in VARCHAR2 DEFAULT NULL, v_schema IN VARCHAR2 DEFAULT USER) IS
   c_cmdname        CONSTANT VARCHAR2(14) := 'SetDefaultCS';
   c_feedback1      CONSTANT VARCHAR2(40) := 'Updated existing default CS for: ';
   c_feedback2      CONSTANT VARCHAR2(40) := 'Inserted new default CS for: ';
   c_feedback3      CONSTANT VARCHAR2(40) := 'Deleted the default CS for: ';
   --
   v_default        VARCHAR2(128);
   i_count          PLS_INTEGER;
   v_sql            VARCHAR2(512);
  BEGIN
   v_sql:= 'SELECT COUNT(1) FROM GDOSYS.GPARAMETERS WHERE SUBSTR(GPARAMETER,1,INSTR(GPARAMETER,''.'')-1)=:vowner';
   Execute Immediate v_sql Into i_count Using v_schema;
   If v_csguid Is Not Null Then
     IF i_count >0 THEN             -- Default CS exists, update it.
       v_sql:='UPDATE GDOSYS.GPARAMETERS SET GVALUE=:vcsguid WHERE SUBSTR(GPARAMETER,1,INSTR(GPARAMETER,''.'')-1)=:vowner';
  	   Execute Immediate v_sql Using v_csguid, v_schema;
       COMMIT;
       RESPONSE( c_cmdname, c_feedback1 || v_schema);
     ELSE                           -- Need new default, insert one.
       v_default := v_schema ||'.DefaultCoordinateSystem';
       v_sql     :='INSERT INTO GDOSYS.GPARAMETERS(GPARAMETER,GVALUE) VALUES (:vdefault,:vcsguid)';
  	   Execute Immediate v_sql Using v_default, v_csguid;
       COMMIT;
       RESPONSE( c_cmdname, c_feedback2 || v_schema);
     END IF;
   Else                             -- Delete the existing default CS.
     v_sql:='DELETE FROM GDOSYS.GPARAMETERS WHERE SUBSTR(GPARAMETER,1,INSTR(GPARAMETER,''.'')-1)=:vowner';
  	 Execute Immediate v_sql Using v_schema;
  	 Commit;
  	 RESPONSE( c_cmdname, c_feedback3 || v_schema);
   End If; 
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, v_schema ||':'|| v_csguid, v_sql,sqlcode,sqlerrm);
  END SetDefaultCS; 
  --------------------------------------------------------------------------------------
  -- SetPrimaryGeom sets the primary geometry indication in GDOSYS for the specified geometry.
  -- Syntax: EXEC GOOM.SetPrimaryGeom(v_tablename, v_geomcol); 
  --         v_tablename : can be owner.table or just table.
  --         v_geomcol   : the name of the geometry in v_tablename that will be primary.
  -- Note: This is not required for tables that contain only one geometry.
  --
  PROCEDURE SetPrimaryGeom( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2) IS
    c_cmdname           CONSTANT VARCHAR2(15) := 'SetPrimaryGeom';
    --
    v_ownertable        VARCHAR2(61);
    v_schema            VARCHAR2(30);
    v_sql               VARCHAR2(155);
  BEGIN
    v_ownertable := GetOwnerObject( v_tablename);
    v_schema     := SplitOwnerObject( v_ownertable, 'OWNER');
    --
    IF NOT ChkTable( v_ownertable ) AND NOT ChkView( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    v_sql:='UPDATE GDOSYS.GFEATURES SET PRIMARYGEOMETRYFIELDNAME=UPPER(:geomcol) where FEATURENAME=UPPER(:ownertable)';
    EXECUTE IMMEDIATE v_sql USING v_geomcol, v_ownertable;
    Response(c_cmdname,'Primary Geometry now set to '|| v_geomcol ||' for feature class '||v_ownertable||'.');
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_ownertable, c_msgGoomInternal);
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, v_tablename ||'.'|| v_geomcol, v_sql,sqlcode,sqlerrm);
  END SetPrimaryGeom;
  --------------------------------------------------------------------------------------
  -- OverrideNumDatatype allows you to set the data type matching override in GDOSYS.GFIELDMAPPING
  -- for the specified table/column.
  -- Syntax: EXEC GOOM.OverrideNumDatatype(v_tablename, v_column, v_type); 
  --         v_type : can be 1 for BOOLEAN, 3 for INTEGER, 4 for LONG, or 7 for DOUBLE
  -- Note:  This only works for numeric data types.
  --
  PROCEDURE OverrideNumDatatype( v_tablename IN VARCHAR2, v_column IN VARCHAR2, v_type IN VARCHAR2)IS
    c_cmdname       CONSTANT VARCHAR2(20):= 'OverrideNumDatatype';
    --
    v_Chk           PLS_INTEGER;
    v_sql           VARCHAR2(1024);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    v_table         VARCHAR2(30);
    v_col           VARCHAR2(30);
    i_colID         INTEGER      := 0;
    v_ovType        PLS_INTEGER  := 0;
    v_ovFormat      VARCHAR2(15) := 'General Number';
  BEGIN
    v_col        := UPPER( v_column );
    v_ownertable := GetOwnerObject( v_tablename );
    v_schema      := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      := SplitOwnerObject( v_ownertable, 'TABLE');
    IF NOT ChkTable( v_ownertable ) AND NOT ChkView( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    --
    IF v_type in ('1','3','4','7') THEN
      v_ovType := TO_NUMBER( v_type );
      IF v_ovType = 1 THEN
          v_ovFormat := 'True/False';
      END IF;
    ELSE
      CASE UPPER(v_type)
        WHEN 'BOOLEAN' THEN
          v_ovType    := 1;
          v_ovFormat := 'True/False';
        WHEN 'INTEGER' THEN
          v_ovType := 3;
        WHEN 'LONG'    THEN
          v_ovType := 4;
        WHEN 'DOUBLE'  THEN
          v_ovType := 7;
        ELSE
          RAISE e_InvalidInput;
      END CASE;
    END IF;
    --
    v_sql:='SELECT COUNT(*) FROM GDOSYS.GFIELDMAPPING WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND COLUMN_NAME=:vcolumn';
    EXECUTE IMMEDIATE v_sql INTO v_Chk USING v_schema, v_table, v_col;
    IF v_Chk > 0 THEN
       v_sql := 'UPDATE GDOSYS.GFIELDMAPPING SET DATA_TYPE = :vovtyp WHERE OWNER=:vowner AND TABLE_NAME=:vtable AND COLUMN_NAME=:vcolumn';
       EXECUTE IMMEDIATE v_sql USING v_ovType, v_schema, v_table, v_col;
    ELSE
       v_sql :='INSERT INTO GDOSYS.GFIELDMAPPING VALUES(:vowner,:vtable,:vcolumn,:vovtyp, NULL, NULL, NULL, NULL)';
       EXECUTE IMMEDIATE v_sql USING  v_schema, v_table, v_col, v_ovType;
    END IF;
    v_sql := 'SELECT INDEXID FROM GDOSYS.FIELDLOOKUP WHERE FEATURENAME=:vownertable AND FIELDNAME=:vcolumn';
    EXECUTE IMMEDIATE v_sql INTO i_colID USING v_ownertable, v_col;
    v_sql := 'UPDATE GDOSYS.ATTRIBUTEPROPERTIES SET FIELDTYPE = :vovtyp WHERE INDEXID = :i_colID';
    EXECUTE IMMEDIATE v_sql USING v_ovType, i_colID;
    v_sql := 'UPDATE GDOSYS.ATTRIBUTEPROPERTIES SET FIELDFORMAT = :vovformat WHERE INDEXID = :i_colID';
    EXECUTE IMMEDIATE v_sql USING v_ovFormat, i_colID;
    COMMIT;
  EXCEPTION
    WHEN e_InvalidInput THEN
      RESPONSE ( c_cmdname || c_msgError, c_msgInvalidInput || v_type);
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgTableNotFound || v_ownertable);
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, v_ownertable ||'.'|| v_col, v_sql,sqlcode,sqlerrm);
      ROLLBACK;
  END OverrideNumDatatype;
  --------------------------------------------------------------------------------------
  -- SetField2Hypertext allows you to set a field format to HyperText for the specified 
  -- table/column for use in GeoMedia.  The command modifies existing GDOSYS metadata.
  -- Syntax: EXEC GOOM.SetField2Hypertext(v_tablename, v_column,); 
  -- Note:  This only works for VARCHAR2 and CHAR data types.
  PROCEDURE SetField2Hypertext( v_tablename IN VARCHAR2, v_column IN VARCHAR2) IS
    c_cmdname       CONSTANT VARCHAR2(20):= 'SetField2Hypertext';
    --
    v_sql           VARCHAR2(1024);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_col           VARCHAR2(30);
    v_coltype       VARCHAR2(40);
    i_indexid       INTEGER      := 0;
    v_ovType        PLS_INTEGER  := 10;
    v_ovFormat      VARCHAR2(15) := 'Hypertext';
  BEGIN
    v_col        := UPPER( v_column );
    v_ownertable := GetOwnerObject( v_tablename );
    -- Verify inputs
    IF NOT chkColumn( v_ownertable, v_column ) THEN
      RAISE e_ColumnNotFound;
    END IF;
    IF NOT ChkMetadata( v_ownertable ) THEN
      RAISE e_NoGDOSYSMetadata;
    END IF;
    v_coltype:=GetColumnType( v_ownertable, v_column);
    IF SUBSTR( v_coltype,1,INSTR( v_coltype,'(',1 )-1) NOT IN ('VARCHAR2','CHAR') THEN
      RAISE e_InvalidDataType;
    END IF;
    -- Update GDOSYS metadata for hypertext
    v_sql := 'SELECT INDEXID FROM GDOSYS.FIELDLOOKUP WHERE FEATURENAME=:vownertable AND FIELDNAME=:vcolumn';
    EXECUTE IMMEDIATE v_sql INTO i_indexid USING v_ownertable, v_col;
    v_sql := 'UPDATE GDOSYS.ATTRIBUTEPROPERTIES SET FIELDTYPE = :vovtyp WHERE INDEXID = :i_indexid';
    EXECUTE IMMEDIATE v_sql USING v_ovType, i_indexid;
    v_sql := 'UPDATE GDOSYS.ATTRIBUTEPROPERTIES SET FIELDFORMAT = :vovformat WHERE INDEXID = :i_indexid';
    EXECUTE IMMEDIATE v_sql USING v_ovFormat, i_indexid;
    COMMIT;
  EXCEPTION
    WHEN e_ColumnNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgColumnNotFound || v_column);
    WHEN e_NoGDOSYSMetadata THEN
      Response( c_cmdname || c_msgerror, c_msgNoGDOSYSMetadata || v_ownertable);
    WHEN e_InvalidDataType THEN
      Response( c_cmdname || c_msgerror, c_msgInvalidDataType || v_ownertable ||'.'|| v_column);
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, v_ownertable ||'.'|| v_col, v_sql, sqlcode, sqlerrm);
      ROLLBACK;
  END SetField2Hypertext;
  --------------------------------------------------------------------------------------
  -- DelGDOSYSMetadata deletes all the GDOSYS metadata associated with the specified table/view.
  -- Syntax: EXEC GOOM.DelGDOSYSMetadata(v_table_name);
  --         v_tablename can be passed as a table/view for the current user.  A DBA user can
  --         pass owner.table. 
  --
  PROCEDURE DelGDOSYSMetadata( v_tablename IN VARCHAR2, b_respn IN BOOLEAN DEFAULT TRUE) IS
    c_cmdname       CONSTANT VARCHAR2(32):= 'DelGDOSYSMetadata';
    c_cmdtype       CONSTANT VARCHAR2(8) := 'GDOSYS';
    c_feedback1     CONSTANT VARCHAR2(36):= 'GDOSYS Metadata does not exist for: ';
    c_feedback2     CONSTANT VARCHAR2(29):= 'GDOSYS Metadata deleted for: ';
    --
    i_count         PLS_INTEGER := 0;
    i_delcount      INTEGER     := 0;
    v_fieldlookup   VARCHAR2(64);
    v_geometryprop  VARCHAR2(64);
    v_attributeprop VARCHAR2(64);
    v_gfieldmapping VARCHAR2(64);
    v_gfeatures     VARCHAR2(64);
    v_indexcols     VARCHAR2(64);
    v_picklists     VARCHAR2(64);
    v_libTables     VARCHAR2(64);
    v_gqueue        VARCHAR2(64);
    v_sql           VARCHAR2(2048);
    v_debug         VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema        VARCHAR2(30);
    v_table         VARCHAR2(30);
  BEGIN
    v_fieldlookup   := GetGDOTablename('INGRFieldLookup');
    v_gfieldmapping := GetGDOTablename('GOracleFieldMapping');
    v_attributeprop := GetGDOTablename('INGRAttributeProperties');
    v_geometryprop  := GetGDOTablename('INGRGeometryProperties');
    v_gfeatures     := GetGDOTablename('INGRFeatures');
    v_picklists     := GetGDOTablename('INGRPickLists');
    v_indexcols     := GetGDOTablename('GOracleIndexColumns');
    v_libTables     := GetGDOTablename('LibraryTables');
    v_gqueue        := GetGDOTablename('GQueue');
    --
    v_ownertable := GetOwnerObject( v_tablename );
    v_schema     := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      := SplitOwnerObject( v_ownertable, 'TABLE'); 
    --
    IF NOT ChkTable( v_ownertable ) AND NOT ChkView( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    v_debug := 'Deleting from GFeatures...';
    v_sql   := 'SELECT COUNT(*) FROM ' || v_gfeatures || ' WHERE FEATURENAME = ''' || v_ownertable || '''';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    IF i_count > 0 THEN
      EXECUTE IMMEDIATE 'DELETE ' || v_gfeatures || ' WHERE FEATURENAME = ''' || v_ownertable || '''';  
      i_delcount := i_delcount + i_count;  
      --Response(c_cmdname,'Records deleted from ' || v_gfeatures || ' for ' || v_ownertable||':'||i_count);
    END IF;
    --
    v_debug := 'Deleting from AttributeProperties...';
    v_sql   := 'SELECT COUNT(*) FROM ' || v_attributeprop || ' WHERE INDEXID IN (SELECT INDEXID FROM ' || v_fieldlookup || ' WHERE FEATURENAME = ''' || v_ownertable || ''')';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    IF i_count > 0 THEN
      EXECUTE IMMEDIATE 'DELETE ' || v_attributeprop || ' WHERE INDEXID IN (SELECT INDEXID FROM ' || v_fieldlookup || ' WHERE FEATURENAME = ''' || v_ownertable || ''')';
      i_delcount := i_delcount + i_count; 
      --Response(c_cmdname,'Records deleted from ' || v_attributeprop || ' for ' || v_ownertable||':'||i_count);
    END IF;
    --
    v_debug := 'Deleting from GeometryProperties...';
    v_sql   := 'SELECT COUNT(*) FROM ' || v_geometryprop || ' WHERE INDEXID IN (SELECT INDEXID FROM ' || v_fieldlookup || ' WHERE FEATURENAME = ''' || v_ownertable || ''')';
    EXECUTE IMMEDIATE v_sql
      INTO i_count;
    IF i_count > 0 THEN
      EXECUTE IMMEDIATE 'DELETE ' || v_geometryprop || ' WHERE INDEXID IN (SELECT INDEXID FROM ' || v_fieldlookup || ' WHERE FEATURENAME = ''' || v_ownertable || ''')';
      i_delcount := i_delcount + i_count;      
      --Response(c_cmdname,'Records deleted from ' || v_geometryprop || ' for ' || v_ownertable||':'||i_count);
    END IF;
    --
    v_debug := 'Deleting from Fieldlookup...';
    v_sql   := 'SELECT COUNT(*) FROM ' || v_fieldlookup || ' WHERE FEATURENAME = ''' || v_ownertable || '''';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    IF i_count > 0 THEN
      EXECUTE IMMEDIATE 'DELETE ' || v_fieldlookup || ' WHERE FEATURENAME = ''' || v_ownertable || '''';
      i_delcount := i_delcount + i_count;        
      --Response(c_cmdname,'Records deleted from ' || v_fieldlookup || ' for ' || v_ownertable||':'||i_count);     
    END IF;
    --
    v_debug := 'Deleting from GFieldmapping...';
    v_sql   := 'SELECT COUNT(*) FROM ' || v_gfieldmapping || ' WHERE OWNER = ''' || v_schema || ''' AND TABLE_NAME = ''' || v_table || '''';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    IF i_count > 0 THEN
      EXECUTE IMMEDIATE 'DELETE ' || v_gfieldmapping || ' WHERE OWNER = ''' || v_schema || ''' AND TABLE_NAME = ''' || v_table || '''';
      i_delcount := i_delcount + i_count;  
      --Response(c_cmdname,'Records deleted from ' || v_gfieldmapping || ' for ' || v_ownertable||':'||i_count); 
    END IF;
    --
    v_debug := 'Deleting from GIndex_Columns...';
    v_sql   := 'SELECT COUNT(*) FROM  ' || v_indexcols || ' WHERE OWNER = ''' || v_schema || ''' AND OBJECT_NAME = ''' || v_table || '''';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    IF i_count > 0 THEN
      EXECUTE IMMEDIATE 'DELETE ' || v_indexcols || ' WHERE OWNER = ''' || v_schema || ''' AND OBJECT_NAME = ''' || v_table || '''';   
      i_delcount := i_delcount + i_count;        
      --Response(c_cmdname,'Records deleted from ' || v_indexcols || ' for ' || v_ownertable||':'||i_count);  
    END IF;
    --
    IF v_picklists IS NOT NULL THEN
      v_debug := 'Deleting from GPickLists...';
      v_sql   := 'SELECT COUNT(*) FROM  ' || v_picklists || ' WHERE FEATURENAME = ''' || v_ownertable || '''';
      EXECUTE IMMEDIATE v_sql INTO i_count;
      IF i_count > 0 THEN
        EXECUTE IMMEDIATE 'DELETE ' || v_picklists || ' WHERE FEATURENAME = ''' || v_ownertable || '''';  
        i_delcount := i_delcount + i_count;          
        --Response(c_cmdname,'Records deleted from ' || v_picklists || ' for ' || v_ownertable||':'||i_count); 
      END IF;
    END IF;
    --
    IF v_libTables IS NOT NULL THEN
      v_debug := 'Deleting from LibraryTable...';
      v_sql   := 'SELECT COUNT(*) FROM  ' || v_libTables || ' WHERE TABLENAME = ''' || v_ownertable || '''';
      EXECUTE IMMEDIATE v_sql INTO i_count;
      IF i_count > 0 THEN
        EXECUTE IMMEDIATE 'DELETE ' || v_libTables || ' WHERE TABLENAME = ''' || v_ownertable || '''';  
        i_delcount := i_delcount + i_count;          
        --Response(c_cmdname,'Records deleted from ' || v_picklists || ' for ' || v_ownertable||':'||i_count); 
      END IF;
    END IF;
    --     
    IF v_gqueue IS NOT NULL THEN
      v_debug := 'Deleting from GQueue...';
      v_sql   := 'SELECT COUNT(*) FROM  ' || v_gqueue || ' WHERE TABLENAME = ''' || v_ownertable || '''';
      EXECUTE IMMEDIATE v_sql INTO i_count;
      IF i_count > 0 THEN
        EXECUTE IMMEDIATE 'DELETE ' || v_gqueue || ' WHERE TABLENAME = ''' || v_ownertable || '''';  
        i_delcount := i_delcount + i_count;          
        --Response(c_cmdname,'Records deleted from ' || v_picklists || ' for ' || v_ownertable||':'||i_count); 
      END IF;
    END IF;
    -- v_gqueue
    v_debug := 'GDOSYS Metadata Deleted.';
    COMMIT;
    IF b_respn THEN
      IF i_delcount = 0 THEN
        Response( c_cmdname, c_feedback1 || v_ownertable); 
        WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable, c_feedback1|| v_ownertable);
      ELSE
        Response( c_cmdname, c_feedback2 || v_ownertable ||': '|| i_delcount); 
        WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_feedback2 || v_ownertable, i_delcount);
      END IF;
    END IF;
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_ownertable, c_msgGoomInternal);
    WHEN OTHERS THEN
      -- Handle Errors by reporting them back to the user.
      ROLLBACK;
      REPORT_ERROR( c_cmdname, v_ownertable, v_debug, v_sql, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable, c_msgOraError || SQLCODE);
  END DelGDOSYSMetadata;
  -------------------------------------------------------------------------------------- 
  -- DelGDOSYSOrphans searches for orphan entries in GDOSYS related to the specified schema.
  -- Syntax: EXEC GOOM.DelGDOSYSOrphans; or EXEC GOOM.DelGDOSYSOrphans(v_schema);
  --         v_schema : optional, normally the current user is used.  A DBA can specify 
  --         any schema.
  --
  PROCEDURE DelGDOSYSOrphans( v_schema IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(30) := 'DelGDOSYSOrphans';
    c_feedback      CONSTANT VARCHAR2(25) := 'Orphan records deleted: ';
    c_feedback2     CONSTANT VARCHAR2(20) := 'No orphans records.';
    --
    v_sql           VARCHAR2(1024);
    v_debug         VARCHAR2(64);
    i_count         INTEGER:=0;
  BEGIN
    DashLine;
    Response( c_cmdname, c_msgStart || v_schema,25);
    v_debug:='GFEATURESBASE';
    v_sql :='SELECT COUNT(1) FROM GDOSYS.GFEATURESBASE WHERE SUBSTR(FEATURENAME,INSTR(FEATURENAME,''.'',1)+1,LENGTH(FEATURENAME)) 
             NOT IN (SELECT OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER=:vowner) AND SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,''.'',1)-1)=:vowner';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_schema;    
    v_sql :='Delete FROM GDOSYS.GFEATURESBASE WHERE SUBSTR(FEATURENAME,INSTR(FEATURENAME,''.'',1)+1,LENGTH(FEATURENAME)) 
             NOT IN (SELECT OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER=:vowner) AND SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,''.'',1)-1)=:vowner';
    EXECUTE IMMEDIATE v_sql USING v_schema, v_schema;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback|| i_count, 35);
    END IF;
    --
    v_debug:='GFIELDMAPPING - ';
    v_sql :='SELECT COUNT(1) FROM GDOSYS.GFIELDMAPPING WHERE OWNER = :vowner
             AND TABLE_NAME NOT IN (SELECT OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER=:vowner)';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_schema; 
    v_sql :='Delete FROM GDOSYS.GFIELDMAPPING WHERE OWNER = :vowner
             AND TABLE_NAME NOT IN (SELECT OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER=:vowner)';
    EXECUTE IMMEDIATE v_sql USING v_schema, v_schema;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug:='FIELDLOOKUP - ';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.FIELDLOOKUP WHERE SUBSTR(FEATURENAME,INSTR(FEATURENAME,''.'',1)+1,LENGTH(FEATURENAME)) 
              NOT IN (SELECT OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER = :vowner) 
              AND SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,''.'',1)-1) = :vowner';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema,v_schema;
    v_sql := 'Delete  FROM GDOSYS.FIELDLOOKUP WHERE SUBSTR(FEATURENAME,INSTR(FEATURENAME,''.'',1)+1,LENGTH(FEATURENAME)) 
              NOT IN (SELECT OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER = :vowner) 
              AND SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,''.'',1)-1) = :vowner';
    EXECUTE IMMEDIATE v_sql USING v_schema, v_schema;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug:='ATTRIBUTEPROPERTIES';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.ATTRIBUTEPROPERTIES WHERE INDEXID NOT IN (SELECT INDEXID FROM GDOSYS.FIELDLOOKUP)';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    v_sql := 'Delete FROM GDOSYS.ATTRIBUTEPROPERTIES WHERE INDEXID NOT IN (SELECT INDEXID FROM GDOSYS.FIELDLOOKUP)';
    EXECUTE IMMEDIATE v_sql;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug:='GEOMETRYPROPERTIES';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.GEOMETRYPROPERTIES WHERE INDEXID NOT IN (SELECT INDEXID FROM GDOSYS.FIELDLOOKUP)';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    v_sql := 'Delete FROM GDOSYS.GEOMETRYPROPERTIES WHERE INDEXID NOT IN (SELECT INDEXID FROM GDOSYS.FIELDLOOKUP)';
    EXECUTE IMMEDIATE v_sql;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug:='GINDEX_COLUMNS';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.GINDEX_COLUMNS A WHERE OBJECT_NAME 
              NOT IN (SELECT OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER=:vowner) AND OWNER = :vowner ';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema,v_schema;
    v_sql := 'Delete FROM GDOSYS.GINDEX_COLUMNS WHERE OBJECT_NAME 
              NOT IN (SELECT OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER=:vowner) AND OWNER = :vowner ';
    EXECUTE IMMEDIATE v_sql USING v_schema, v_schema;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug:='LIBRARYTABLESBASE';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.LIBRARYTABLESBASE A WHERE A.TABLENAME
              NOT IN (SELECT OWNER||''.''||OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER=:vowner) 
              AND SUBSTR(A.TABLENAME,1,INSTR(A.TABLENAME,''.'',1)-1) = :vowner';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_schema;
    v_sql := 'Delete FROM GDOSYS.LIBRARYTABLESBASE A WHERE A.TABLENAME
              NOT IN (SELECT OWNER||''.''||OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER=:vowner) 
              AND SUBSTR(A.TABLENAME,1,INSTR(A.TABLENAME,''.'',1)-1) = :vowner';
    EXECUTE IMMEDIATE v_sql USING v_schema,v_schema;
    --
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback|| i_count, 35);
    END IF;
    --
    v_debug:='GQUEUEBASE';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.GQUEUEBASE A WHERE A.TABLENAME
              NOT IN (SELECT OWNER||''.''||OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER=:vowner)
              AND SUBSTR(A.TABLENAME,1,INSTR(A.TABLENAME,''.'',1)-1) = :vowner';
    EXECUTE IMMEDIATE v_sql INTO i_count USING v_schema, v_schema;
    v_sql := 'Delete FROM GDOSYS.GQUEUEBASE A WHERE A.TABLENAME
              NOT IN (SELECT OWNER||''.''||OBJECT_NAME FROM ALL_OBJECTS WHERE OWNER=:vowner)
              AND SUBSTR(A.TABLENAME,1,INSTR(A.TABLENAME,''.'',1)-1) = :vowner';
    EXECUTE IMMEDIATE v_sql USING v_schema, v_schema;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    COMMIT;
    DeleteOrphanCS;
    Response( c_cmdname, c_msgComplete || v_schema, 25);
    DashLine;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_schema, v_sql, v_debug, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_sql, c_msgOraError || SQLCODE);
      ROLLBACK;
  END DelGDOSYSOrphans;
  -------------------------------------------------------------------------------------- 
  -- DBADelGDOSYSOrphans is for DBAs only.  It will compare GDOSYS metadata to all cuurent
  -- database schemas and remove any metadata entry that is not related to an existing user.
  -- In essence, this will delete all orphan metadata from GDOSYS.  It is not recoverable 
  -- so make sure you really need to do this.
  -- Syntax: EXEC GOOM.DBADelGDOSYSOrphans;
  --
  PROCEDURE DBADelGDOSYSOrphans IS
    c_cmdname       CONSTANT VARCHAR2(30) := 'DelGDOSYSOrphans';
    c_feedback      CONSTANT VARCHAR2(25) := 'Orphan records deleted: ';
    c_feedback2     CONSTANT VARCHAR2(20) := 'No orphans records.';
    --
    v_sql           VARCHAR2(1024);
    v_debug         VARCHAR2(64);
    i_count         INTEGER:=0;
    v_schema        VARCHAR2(3):='DBA';
  BEGIN
    DashLine;
    Response( c_cmdname, c_msgStart || v_schema,25);
    v_debug:='GFEATURESBASE';
    v_sql :='SELECT COUNT(1) FROM GDOSYS.GFEATURESBASE WHERE SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,''.'',1)-1) NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql INTO i_count;    
    v_sql :='Delete FROM GDOSYS.GFEATURESBASE WHERE SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,''.'',1)-1) NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback|| i_count, 35);
    END IF;
    --
    v_debug:='GFIELDMAPPING - ';
    v_sql :='SELECT COUNT(1) FROM GDOSYS.GFIELDMAPPING WHERE OWNER NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql INTO i_count; 
    v_sql :='Delete FROM GDOSYS.GFIELDMAPPING WHERE OWNER NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug:='FIELDLOOKUP - ';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.FIELDLOOKUP WHERE SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,''.'',1)-1) NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    v_sql := 'Delete  FROM GDOSYS.FIELDLOOKUP WHERE SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,''.'',1)-1) NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug:='ATTRIBUTEPROPERTIES';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.ATTRIBUTEPROPERTIES WHERE INDEXID NOT IN (SELECT INDEXID FROM GDOSYS.FIELDLOOKUP)';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    v_sql := 'Delete FROM GDOSYS.ATTRIBUTEPROPERTIES WHERE INDEXID NOT IN (SELECT INDEXID FROM GDOSYS.FIELDLOOKUP)';
    EXECUTE IMMEDIATE v_sql;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug:='GEOMETRYPROPERTIES';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.GEOMETRYPROPERTIES WHERE INDEXID NOT IN (SELECT INDEXID FROM GDOSYS.FIELDLOOKUP)';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    v_sql := 'Delete FROM GDOSYS.GEOMETRYPROPERTIES WHERE INDEXID NOT IN (SELECT INDEXID FROM GDOSYS.FIELDLOOKUP)';
    EXECUTE IMMEDIATE v_sql;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug:='GINDEX_COLUMNS';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.GINDEX_COLUMNS A WHERE OWNER NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    v_sql := 'Delete FROM GDOSYS.GINDEX_COLUMNS WHERE OWNER NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug:='LIBRARYTABLESBASE';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.LIBRARYTABLESBASE A WHERE SUBSTR(A.TABLENAME,1,INSTR(A.TABLENAME,''.'',1)-1) NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    v_sql := 'Delete FROM GDOSYS.LIBRARYTABLESBASE A WHERE SUBSTR(A.TABLENAME,1,INSTR(A.TABLENAME,''.'',1)-1) NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql;
    --
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback|| i_count, 35);
    END IF;
    --
    v_debug:='GQUEUEBASE';
    v_sql := 'SELECT COUNT(1) FROM GDOSYS.GQUEUEBASE A WHERE SUBSTR(A.TABLENAME,1,INSTR(A.TABLENAME,''.'',1)-1) NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql INTO i_count;
    v_sql := 'Delete FROM GDOSYS.GQUEUEBASE A WHERE SUBSTR(A.TABLENAME,1,INSTR(A.TABLENAME,''.'',1)-1) NOT IN (SELECT USERNAME FROM ALL_USERS)';
    EXECUTE IMMEDIATE v_sql;
    IF i_count = 0 THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSE
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    COMMIT;
    DeleteOrphanCS;
    Response( c_cmdname, c_msgComplete || v_schema, 25);
    DashLine;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_schema, v_sql, v_debug, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_sql, c_msgOraError || SQLCODE);
      ROLLBACK;
  END DBADelGDOSYSOrphans;
  --------------------------------------------------------------------------------------
  -- DeleteOrphanCS deletes orphan coordinate systems from GDOSYS.GCOORDSYSTEM.  If the 
  -- the coordinate system is used anywhere in GDOSYS, it will not be deleted.
  -- Syntax: EXEC GOOM.DeleteOrphanCS;
  --   
  PROCEDURE DeleteOrphanCS ( b_respn in BOOLEAN DEFAULT TRUE) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'DeleteOrphanCS';
    c_feedback      CONSTANT VARCHAR2(25) := 'Orphan records deleted: ';
    c_feedback2     CONSTANT VARCHAR2(20) := 'No orphans records.';
    --
    i_count     PLS_INTEGER  := 0;
    v_debug     VARCHAR2(64);
  BEGIN
    v_debug := 'GCOORDSYSTEM';
    SELECT COUNT(1)
      INTO i_count
      FROM GDOSYS.GCOORDSYSTEM
     WHERE CSGUID NOT IN (SELECT CSGUID FROM GDOSYS.GFIELDMAPPING WHERE CSGUID IS NOT NULL)
       AND CSGUID NOT IN (SELECT GVALUE FROM GDOSYS.GPARAMETERS);
    DELETE FROM GDOSYS.GCOORDSYSTEM
     WHERE CSGUID NOT IN (SELECT CSGUID FROM GDOSYS.GFIELDMAPPING WHERE CSGUID IS NOT NULL)
       AND CSGUID NOT IN (SELECT GVALUE FROM GDOSYS.GPARAMETERS);
    IF ( b_respn) AND ( i_count = 0) THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSIF ( b_respn ) AND i_count > 0 THEN
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug := 'GPARAMETERS (GUIDS)';
    SELECT COUNT(1)
      INTO i_count
      FROM GDOSYS.GPARAMETERS
     WHERE GVALUE NOT IN (SELECT CSGUID FROM GDOSYS.GCOORDSYSTEM)
       AND GVALUE LIKE '{%';
    DELETE FROM GDOSYS.GPARAMETERS
     WHERE GVALUE NOT IN (SELECT CSGUID FROM GDOSYS.GCOORDSYSTEM)
       AND GVALUE LIKE '{%';
    IF ( b_respn ) AND ( i_count = 0 ) THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSIF ( b_respn ) AND i_count > 0 THEN
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    v_debug := 'GPARAMETERS (USERS)';
    SELECT COUNT(1)
      INTO i_count
      FROM GDOSYS.GPARAMETERS
     WHERE SUBSTR(GPARAMETER, 1, INSTR(GPARAMETER, '.') - 1) IS NOT NULL
       AND SUBSTR(GPARAMETER, 1, INSTR(GPARAMETER, '.') - 1) NOT IN (SELECT USERNAME FROM ALL_USERS);
    --
    DELETE FROM GDOSYS.GPARAMETERS
     WHERE SUBSTR(GPARAMETER, 1, INSTR(GPARAMETER, '.') - 1) IS NOT NULL
       AND SUBSTR(GPARAMETER, 1, INSTR(GPARAMETER, '.') - 1) NOT IN (SELECT USERNAME FROM ALL_USERS);
    IF ( b_respn ) AND ( i_count = 0) THEN
      RESPONSE( v_debug, c_feedback2, 35);
    ELSIF ( b_respn ) AND i_count > 0 THEN
      RESPONSE( v_debug, c_feedback || i_count, 35);
    END IF;
    --
    COMMIT;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR(c_cmdname, 'GDOSYS.GCOORDSYSTEM', c_feedback || i_count, SQLCODE, SQLERRM);
  END DeleteOrphanCS;
  -------------------------------------------------------------------------------------- 
  -- CleanModLog is used to truncate the GDOSYS.ModificationLog and ModifiedTables tables.
  -- Syntax: EXEC GOOM.ClearModLog;
  --         Generally this should be run by a DBA user or as a system level job.  An option 
  --         was added to allow EXEC GOOM.ClearModLog(USER); but this will be slow.  
  --
  PROCEDURE ClearModLog ( v_user IN VARCHAR2 DEFAULT NULL) IS
    c_cmdname   CONSTANT VARCHAR2(32) := 'ClearModLog';
    c_error1    CONSTANT VARCHAR2(32) := 'ModificationLog NOT cleared.';
    c_error2    CONSTANT VARCHAR2(32) := 'Try EXEC GOOM.ClearModLog(USER);';
    c_feedback  CONSTANT VARCHAR2(22) := 'Records deleted from ';
    c_sep       CONSTANT VARCHAR2(2)  :=': ';
    --
    v_logtable  VARCHAR2(30);
    v_modtable  VARCHAR2(30);
    i_count     PLS_INTEGER;
    v_sql       VARCHAR2(255);
  BEGIN
    -- Get metadata table names
    v_logtable:=GetGDOTableName('GModifications');
    v_modtable:=GetGDOTableName('GModifiedTables');
    -- Clear GModifications
    v_sql:='SELECT COUNT(1) FROM '|| v_logtable;
    EXECUTE IMMEDIATE v_sql INTO i_count;
    IF v_user IS NULL THEN
      v_sql:='TRUNCATE TABLE '|| v_logtable;
    ELSE
      v_sql:='DELETE FROM '|| v_logtable;
    END IF;
    EXECUTE IMMEDIATE v_sql;
    COMMIT;
    Response( c_cmdname, c_feedback || v_logtable || c_sep || i_count);
    -- Clear GModifiedTables
    v_sql:='SELECT COUNT(1) FROM '|| v_modtable;
    EXECUTE IMMEDIATE v_sql INTO i_count;
    IF v_user IS NULL THEN
      v_sql:='TRUNCATE TABLE '|| v_modtable;
    ELSE
      v_sql:='DELETE FROM '|| v_modtable;
    END IF;
    EXECUTE IMMEDIATE v_sql;
    COMMIT;
    Response( c_cmdname, c_feedback || v_modtable || c_sep || i_count);
  EXCEPTION
    WHEN e_NoPrivilege THEN
      Response( c_cmdname || c_msgError, c_msgNoPrivilege || c_error2);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, c_error1, v_sql, SQLCODE, SQLERRM);
  END ClearModLog;
  -------------------------------------------------------------------------------------- 
  -- LogDataModification will log and inset, update, or delete operation into GDOSYS's
  -- MODIFIEDTABLES and MODIFICATIONLOG tables.  This is useful when writing modlog
  -- triggers or doing customization that requires external notification for GM users.
  -- Syntax: EXEC GOOM.LogDataModification(v_tablename, v_keyname, i_keyvalue, i_modtype);
  --         v_tablename : the table being modified.
  --         v_keyname   : the primary key column name for the table.
  --         i_keyvalue  : the primary key row identifier being modified.
  --         i_modtype   : the type of modification:  1 for insert, 2 for update, and 3 for delete.
  -- Note:  The procedure only works with single, numeric based primary keys.
  --
  PROCEDURE LogDataModification( v_tablename IN VARCHAR2, v_keyname IN VARCHAR2, v_keyvalue IN VARCHAR2, i_modtype IN INTEGER) IS
    c_cmdname     CONSTANT VARCHAR2(20) := 'LogDataModification';
    --
    v_debug       VARCHAR2(30) := 'Init';
    v_sessionID   INTEGER      := USERENV('SESSIONID'); 
    v_ownertable  VARCHAR2(61);
    v_table       VARCHAR2(30);
    i_ModID       INTEGER;
    i_objectID    INTEGER;
    i_count       PLS_INTEGER;
  BEGIN
    v_debug      := 'Start';
    v_ownertable := GetOwnerObject( v_tablename );
    v_table      := SplitOwnerObject( v_ownertable, 'OBJECT'); 
    -- Check to see if table or view has been used before.
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM GDOSYS.MODIFIEDTABLES WHERE TABLENAME = :ownertable '
       INTO i_count USING v_ownertable;
    IF i_count = 0 THEN  -- if not, then get the object id and add it to MODIFIEDTABLES.
      v_debug := 'Getting New OBJ ID';
      EXECUTE IMMEDIATE 'SELECT OBJECT_ID FROM USER_OBJECTS WHERE OBJECT_NAME = :objname and OBJECT_TYPE IN (''TABLE'',''VIEW'')'
         INTO i_objectID USING v_table;
      EXECUTE IMMEDIATE 'INSERT INTO GDOSYS.MODIFIEDTABLES (MODIFIEDTABLEID,TABLENAME,KEYFIELD1) VALUES (:TableID,:ownertable,:keyfield)'
        USING i_objectID, v_ownertable, v_keyname;
    ELSE  -- if it has been used, get the current object id.
      v_debug := 'Getting existing OBJ ID';
      EXECUTE IMMEDIATE 'SELECT MODIFIEDTABLEID FROM GDOSYS.MODIFIEDTABLES WHERE TABLENAME = :ownertable'
         INTO i_objectID USING v_ownertable;
    END IF;
    -- Insert a ModLog entry for this feature class (either view or table).
    v_debug := 'Inserting Log Entry';
    EXECUTE IMMEDIATE 'SELECT GDOSYS.GMODLOG.NEXTVAL FROM DUAL' INTO i_ModID;
    EXECUTE IMMEDIATE 'INSERT INTO GDOSYS.MODIFICATIONLOG (MODIFICATIONNUMBER, SESSIONID, "TYPE" ,MODIFIEDTABLEID, MODIFIEDDATE, KEYVALUE1) VALUES (:1,:2,:3,:4,:5, :6)'
      USING i_ModID, v_sessionID, i_modtype, i_objectID, SYSDATE, v_keyvalue;
    -- ModLog update complete.  Handle exceptions.
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, v_keyname, v_debug, SQLERRM);
  END LogDataModification;

  -- -----------------------------------------------------------------------------------------------------------------
  -- GEOMETRY VALIDATION AND REPAIR PROCEDURES: Data Validation
  -- -----------------------------------------------------------------------------------------------------------------

  -- ValidateGeom uses Oracle's VALIDATE_LAYER_WITH_CONTEXT to validate the geometry in 
  -- the feature class.  It handles the error table as well as other bookkeeping.
  -- Syntax: EXEC GOOM.ValidateGeom(v_tablename,v_geomcol);
  --         v_tablename : CANNOT use OWNER.TABLE.  The owner of the table must run the command.
  --         v_geomcol   : the geometry column to be validated.
  --
  PROCEDURE ValidateGeom( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'ValidateGeom';
    c_cmdtype       CONSTANT VARCHAR2(10) := 'VALIDATION';
    c_feedback      CONSTANT VARCHAR2(47) := 'No errors found, data passed Oracle validation.';
    c_ErrFound      CONSTANT VARCHAR2(23) := 'Geometry errors found: ';
    c_Results       CONSTANT VARCHAR2(23) := 'Results in table: ';
    c_commit        CONSTANT INTEGER      := 200;
    --
    v_resulttab     VARCHAR2(30);
    i_errcount      INTEGER;
    v_debug         VARCHAR2(255);
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    v_table         VARCHAR2(30);
    v_geom          VARCHAR2(30);
    n_dbversion     NUMBER;
    v_sql           VARCHAR2(512);
  BEGIN
    v_debug      := 'Start';
    v_ownertable := GetOwnerObject( v_tablename );
    v_schema      := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      := SplitOwnerObject( v_ownertable, 'TABLE'); 
    v_resulttab  := RTRIM(SUBSTR( v_table, 1, 26)) || c_valext;
    -- Added to check for 11.2 and validate 3d arcs in this version.
    n_dbversion  := TO_NUMBER(NVL(SUBSTR(GetDBVersion,1,4),'11.2'));
    v_debug      := 'Input Processed';
    -- Check inputs
    IF v_schema != USER THEN
      RAISE e_TabOwnerError;
    END IF;
    IF NOT ChkTable( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    IF chkVIEW( v_table ) THEN
      RAISE e_TableIsView;
    END IF;
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF NOT ChkGeometry( v_table, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    -- Start Process
    v_debug := 'Start Validation Process';
    EXECUTE IMMEDIATE 'UPDATE ' || v_table || ' A SET A.' || v_geom || '= NULL WHERE A.' || v_geom || '.SDO_GTYPE IS NULL';
    COMMIT;
    v_debug := 'Fixed NULL Geoms';
    IF chkTable( v_resulttab ) THEN
      EXECUTE IMMEDIATE 'DROP TABLE ' || v_resulttab||' PURGE';
    END IF;
    EXECUTE IMMEDIATE 'CREATE TABLE ' || v_resulttab || ' (SDO_ROWID ROWID, RESULT VARCHAR2(1000))';
    v_debug := 'Val Table created';
    -- Validate the layer, write results to result table
    /*  This section was the original but makes GOOM incompatible with 10g.
        IF n_dbversion >= 11.2 THEN
          -- If database is 11.2 or later, allow 3D arcs on linestrings to pass validation.
          SDO_GEOM.VALIDATE_LAYER_WITH_CONTEXT( v_table, v_geom, v_resulttab, c_commit, 'TRUE', 'TRUE' );
        ELSE
          SDO_GEOM.VALIDATE_LAYER_WITH_CONTEXT( v_table, v_geom, v_resulttab, c_commit);
        END IF;
    */ 
    -- Instead, we have to wrap the SDO_GEOM procedure in EXECUTE IMMEDIATE so that the procedure is only processed at runtime.  
    IF n_dbversion >= 11.2 THEN
      -- If database is 11.2 or later, allow 3D arcs on linestrings to pass validation.
      v_sql := 'BEGIN SDO_GEOM.VALIDATE_LAYER_WITH_CONTEXT( :v_table, :v_geom, :v_resulttab, :c_commit, ''TRUE'', ''TRUE'' ); END;';
    ELSE
      v_sql := 'BEGIN SDO_GEOM.VALIDATE_LAYER_WITH_CONTEXT( :v_table, :v_geom, :v_resulttab, :c_commit); END;';
    END IF;
    EXECUTE IMMEDIATE v_sql USING v_table, v_geom, v_resulttab, c_commit;
    COMMIT;
    -- End of Validate layer section.
    v_debug := 'Validation Complete';
    -- Clean up the results by deleting null records from result table.
    EXECUTE IMMEDIATE 'DELETE FROM ' || v_resulttab || ' WHERE SDO_ROWID IS NULL';
    EXECUTE IMMEDIATE 'DELETE FROM ' || v_resulttab || ' WHERE RESULT = ''NULL''';
    COMMIT;
    v_debug := 'NULL Results cleared';
    -- Count the errors for reporting and log results
    -- SELECT getcount( v_resulttab ) INTO i_errcount FROM DUAL; -- changed due to 12 priv issue
    EXECUTE IMMEDIATE 'SELECT GOOM.getcount(''' || v_resulttab || ''') FROM DUAL' INTO i_errcount ;
    IF i_errcount = 0 THEN
      Response( c_cmdname, c_feedback);
      WRITE_RESULTS(USER, c_cmdtype, c_cmdname, v_table || '.' || v_geom, c_feedback);
      EXECUTE IMMEDIATE 'DROP TABLE ' || v_resulttab||' PURGE';
    ELSE
      EXECUTE IMMEDIATE 'ALTER TABLE ' || v_resulttab || ' ADD DESCRIPTION VARCHAR2(255)';
      EXECUTE IMMEDIATE 'UPDATE ' || v_resulttab || ' SET DESCRIPTION=GOOM.GetERROR(SUBSTR(RESULT,1,5))';
      COMMIT;
      Response( c_cmdname, c_ErrFound || i_errcount ||', '|| c_Results || v_resulttab);
      WRITE_RESULTS(USER, c_cmdtype, c_cmdname, v_table || '.' || v_geomcol, c_Results || v_resulttab ||'. '|| c_ErrFound || i_errcount);
    END IF;
  EXCEPTION
    WHEN e_TabOwnerError THEN
      Response( c_cmdname || c_msgerror, c_msgTabOwnerError || v_schema ||'<>'||USER);
      WRITE_RESULTS( v_schema, c_msgerror, c_cmdname, c_msgTabOwnerError || v_schema ||'<>'||USER , c_msgGoomInternal);      
    WHEN e_TableIsView THEN
      Response( c_cmdname || c_msgWarning, c_msgTableIsView || v_table);
      WRITE_RESULTS( v_schema, c_msgWarning, c_cmdname, c_msgTableIsView || v_table , c_msgGoomInternal);
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_table ||'.'|| v_geom);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgGeometryNotFound || v_table ||'.'|| v_geom, c_msgGoomInternal);
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_table);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_table, c_msgGoomInternal);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_resulttab, v_table || '.' || v_geom, v_debug, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_table || '.' || v_geom, c_msgOraError || SQLCODE);
  END ValidateGeom;
  --------------------------------------------------------------------------------------
  -- GetError returns a text description of the spatial error code (from ValidateGeom).
  -- Syntax: v_RESULT:=GOOM.GetError(v_error);
  --         SELECT GOOM.GetError(v_error) FROM DUAL;
  -- 
  FUNCTION GetError( v_error VARCHAR2 DEFAULT 'EMPTY') RETURN VARCHAR2 IS
    c_cmdname CONSTANT VARCHAR2(8) := 'GetError';
    v_result  VARCHAR2(255);
  BEGIN
    IF v_error = 'EMPTY' THEN
      v_result := 'No Error Returned';
    ELSE
      v_result := 
       CASE v_error 
       WHEN '13011' THEN 'The Geometry is outside of MBR. Check Geometry Column Range with USER_SDO_GEOM_METADATA' 
       WHEN '13019' THEN 'The specified dimension value is outside the range defined for that dimension.' 
       WHEN '13020' THEN 'An individual value in the ordinate array is NULL. This geometry will need to be repaired or removed.' 
       WHEN '13021' THEN 'The coordinates defining a geometric element are not connected. This geometry will need to be repaired or removed.'  
       WHEN '13022' THEN 'Polygon has a self intersecting geometry. This geometry will need to be repaired or removed.' 
       WHEN '13023' THEN 'An interior element interacts with the exterior element of that object. This geometry will need to be repaired or removed.' 
       WHEN '13024' THEN 'This Polygon has less than three segments.  This geometry will need to be repaired or removed.' 
       WHEN '13025' THEN 'This Polygon does not close.  This geometry will need to be repaired or removed.' 
       WHEN '13026' THEN 'Invalid ETYPE. The SDO_ETYPE column contains an invalid geometric element type value.' 
       WHEN '13027' THEN 'Unable to read dimension definition from string.' 
       WHEN '13028' THEN 'There is an invalid SDO_GTYPE in the SDO_GEOMETRY object.' 
       WHEN '13029' THEN 'There is an invalid SDO_SRID in the SDO_GEOMETRY object.' 
       WHEN '13030' THEN 'There is a mismatch between the dimension in the SDO_GTYPE and the dimension in the USER_SDO_GEOM_METADATA for the SDO_GEOMETRY object.' 
       WHEN '13031' THEN 'The SDO_ELEM_INFO and/or SDO_ORDINATE varrays are NULL but SDO_GTYPE does not indicate a point feature.' 
       WHEN '13032' THEN 'Uninitialized SDO_GEOMETRY object. Use EXEC GOOM.FixNullGeoms; to solve this one.' 
       WHEN '13033' THEN 'Invalid Triplets defined in the SDO_ELEM_INFO varray.' 
       WHEN '13034' THEN 'Invalid values in the SDO_ORDINATE varray.' 
       WHEN '13035' THEN 'Invalid use of arcs in geodetic data (long/lat).  The arcs have to be stroked.' 
       WHEN '13050' THEN 'Unable to construct object, typically indicates a reversed geometry. This geometry will need to be repaired or removed.' 
       WHEN '13051' THEN 'Failed to initialize spatial object.  This one is very ugly. You may need to NULL the geometry for this row.' 
       WHEN '13052' THEN 'Unsupported Geometry Type detected.  Check the data for this row, what are you actually storing?' 
       WHEN '13053' THEN 'The maximum number of geometry elements has been exceeded.' 
       WHEN '13155' THEN 'An invalid number of dimensions has been specified.' 
       WHEN '13340' THEN 'The point contains more than one coordinate pair.  Specify a MultiPoint geometry in this case.' 
       WHEN '13341' THEN 'A linear geometry contains only one point.  Remember, two points are required for a line.' 
       WHEN '13342' THEN 'A arc has been defined with less that 3 points.  Remember, three points are needed to define an arc.' 
       WHEN '13343' THEN 'A polygon geometry has been defined with less than four coordinates.' 
       WHEN '13344' THEN 'A compound polygon containing an arc has been defined with less than five coordinates.' 
       WHEN '13345' THEN 'A compound polygon has been defined with less than five coordinates.' 
       WHEN '13346' THEN 'The geometry for the arc is invalid, an arc is defined using three non-collinear coordinates.' 
       WHEN '13347' THEN 'The arc is collinear, two or more of the three points defining an arc are the same.' 
       WHEN '13348' THEN 'The boundary of a polygon does not close, the first and last point of the polygon must match.' 
       WHEN '13349' THEN 'The Polygon has a self intersecting geometry.' 
       WHEN '13350' THEN 'Two or more rings of a complex polygon touch. All rings of a complex polygon must be disjoint.' 
       WHEN '13351' THEN 'The inner or outer rings of a complex polygon overlap, this is an invalid construct.' 
       WHEN '13353' THEN 'The ELEM_INFO_ARRAY in SDO_GEOMETRY has more or fewer elements than expected.' 
       WHEN '13354' THEN 'The offset field in ELEM_INFO_ARRAY of SDO_GEOMETRY references an invalid array subscript in SDO_ORDINATE_ARRAY.' 
       WHEN '13355' THEN 'The number of elements in SDO_ORDINATE_ARRAY is not a multiple of the number of dimensions supplied by the user.'
       WHEN '13356' THEN 'Redundant Points found, either the tolerance is low or points need thinning' 
       WHEN '13357' THEN 'Extents should be represented by 2 points: lower left and upper right.' 
       WHEN '13359' THEN 'The 2 points representing the extent are identical.' 
       WHEN '13360' THEN 'This subtype is not allowed within the ETYPE specified.' 
       WHEN '13361' THEN 'The compound type declare more sub-elements than actually defined.' 
       WHEN '13362' THEN 'Compound polygon must describe an enclosed area.' 
       WHEN '13363' THEN 'None of the ETYPEs within the geometry is supported.' 
       WHEN '13364' THEN 'Layer Dimensionality does not match geometry dimensions, check SDO_GTYPE against USER_SDO_GEOM_METADATA.' 
       WHEN '13365' THEN 'The SRID used in the geometry column does match the SRID used in USER_SDO_GEOM_METADATA.' 
       WHEN '13366' THEN 'The interior and exterior rings of a polygon geometry are not used consistently.' 
       WHEN '13367' THEN 'The interior/exterior rings in the polygon geometry have incorrect orientation.' 
       WHEN '13368' THEN 'A single polygon definition contains more than one external ring.' 
       WHEN '13369' THEN 'A 4 digit ETYPE has been specified for a non-polygon based geometry.' 
       WHEN '13373' THEN 'A geodetic line segment was greater than half of a great circle, this is illegal.' 
       WHEN '54506' THEN 'The 3-D geometry contains compound curves, which are not supported for 3-D geometries.' 
       WHEN '54530' THEN 'The specified element type is invalid.  This can be caused by 3d arcs in 11g, validate using the flag10G parameter.' 
       WHEN 'DONE ' THEN 'Process Complete' 
       WHEN 'Rows ' THEN 'General Information' 
       WHEN 'NULL ' THEN 'SDO_GEOMETRY is NULL, this is allowed as long as individual SDO Objects are not atomic nulls.' 
       ELSE 'Error Unknown, Look up the error code in the Oracle Error Message Documentation (or GOOGLE).' 
       END;
    END IF;
    RETURN v_result;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_result, 'Error passed:' || v_error, SQLCODE, SQLERRM);
  END GetError;
  --------------------------------------------------------------------------------------
  -- FixRedundantPoints uses Oracle's REMOVE_DUPLICATE_VERTICES to remove redundant points
  -- based on the tolerance value.
  -- Syntax: EXEC GOOM.FixRedundantPoints(v_table_name,v_geomcol,[n_tol]);
  --         n_tol is optional. If not specified, the default of 0.00005 will be used.
  -- Note: Remember that tolerance is based on units of storage.
  --
  PROCEDURE FixRedundantPoints( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, n_tol IN NUMBER DEFAULT 0.00005) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'FixRedundantPoints';
    c_cmdtype       CONSTANT VARCHAR2(10) := 'VALIDATION';
    c_feedback      CONSTANT VARCHAR2(30) := 'Duplicate vertices removed: ';
    --
    v_sql           VARCHAR2(1024);
    v_rowid         ROWID;
    g_geometry      MDSYS.SDO_GEOMETRY;
    i_cur           PLS_INTEGER;
    i_ret           PLS_INTEGER;
    i_count         PLS_INTEGER := 0;
    i_bvert         INTEGER;
    i_avert         INTEGER;
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    v_geom          VARCHAR2(30);
    i_geomchk       INTEGER;
  BEGIN
    v_ownertable := GetOwnerObject(v_tablename);
    v_schema      := SplitOwnerObject(v_ownertable,'OWNER');
    -- Check Inputs
    IF NOT ChkTable( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    IF chkVIEW( v_ownertable ) THEN
      RAISE e_TableIsView;
    END IF;
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF NOT ChkGeometry( v_ownertable, v_geomcol)  OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    --
    v_sql := 'SELECT ROWID FROM ' || v_ownertable; -- Use the rowids to update the table
    --
    i_cur := SYS.DBMS_SQL.OPEN_CURSOR;
    SYS.DBMS_SQL.PARSE( i_cur, v_sql, SYS.DBMS_SQL.NATIVE);
    SYS.DBMS_SQL.DEFINE_COLUMN_ROWID( i_cur, 1, v_rowid);
    i_ret := SYS.DBMS_SQL.EXECUTE( i_cur );
    LOOP
      -- Loop thru table
      IF (SYS.DBMS_SQL.FETCH_ROWS( i_cur) = 0) THEN
        -- exit when all rows retrieved
        EXIT;
      END IF;
      SYS.DBMS_SQL.COLUMN_VALUE_ROWID( i_cur, 1, v_rowid);
      -- Check for NULL GEOMETRY as this will cause GETNUMVERTICES to fail.
      v_sql := 'select COUNT(*) from ' || v_ownertable || ' where ROWID=''' || v_rowid || ''' AND ' || v_geom || ' IS NULL';
      EXECUTE IMMEDIATE v_sql INTO i_geomchk;
      IF i_geomchk = 0 THEN
        v_sql := 'select SDO_UTIL.GETNUMVERTICES(' || v_geom || ') from ' || v_ownertable || ' where ROWID=''' || v_rowid || '''';
        EXECUTE IMMEDIATE v_sql INTO i_bvert;
        v_sql := 'select SDO_UTIL.REMOVE_DUPLICATE_VERTICES(A.' || v_geom || ',' || n_tol || ') from ' || v_ownertable || ' A where ROWID=''' ||v_rowid || '''';
        EXECUTE IMMEDIATE v_sql INTO g_geometry;
        v_sql := 'UPDATE ' || v_ownertable || ' A SET A.' || v_geom || '=:geom where ROWID=''' || v_rowid || '''';
        EXECUTE IMMEDIATE v_sql USING g_geometry;
        COMMIT;
        v_sql := 'select SDO_UTIL.GETNUMVERTICES(' || v_geom || ') from ' || v_ownertable || ' where ROWID=''' || v_rowid || '''';
        EXECUTE IMMEDIATE v_sql INTO i_avert;
        i_count := i_count + ( i_bvert - i_avert ); -- Running total of vertices that have been removed.
      END IF;
    END LOOP;
    SYS.DBMS_SQL.CLOSE_CURSOR( i_cur );
    Response( c_cmdname, c_feedback || i_count);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable || '.' || v_geom, c_feedback || i_count);
  EXCEPTION
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_ownertable ||'.'|| v_geom);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geom, c_msgGoomInternal);
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_ownertable, c_msgGoomInternal);
    WHEN e_TableIsView THEN
      Response( c_cmdname || c_msgError, c_msgTableIsView || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableIsView || v_ownertable, c_msgGoomInternal);
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geom, v_sql, 'ROWID: '|| v_rowid, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geom, c_msgOraError || SQLCODE);
  END FixRedundantPoints;
  --------------------------------------------------------------------------------------
  -- RepairGeometry uses Oracle's Rectify_Geometry to repair the geometry.  Rectify fixes 
  -- redundant points (ORA-13356), reversed polygons (ORA-13367), and self-intersecting 
  -- polygons (ORA-31349).  
  -- Syntax: EXEC GOOM.RepairGeometry(v_tablename,v_geomcol, n_tol);
  --         n_tol : optional. If not specified, the default of 0.00005 will be used.
  -- Note: Remember that tolerance is based on units of storage.  This procedure does not 
  -- support owner.table.  Be careful with this one as it may be data destructive.
  --
  PROCEDURE RepairGeometry( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, n_tol IN NUMBER DEFAULT 0.00005) IS
    c_cmdname           CONSTANT VARCHAR2(15) := 'RepairGeometry';
    c_msgNoValTable     CONSTANT VARCHAR2(26) := 'No Validation Table for: ';
    c_feedback          CONSTANT VARCHAR2(17) := 'Rows Processed: ';
    --
    e_NoValTable        EXCEPTION;
    v_sql               VARCHAR2(1024);
    v_debug             VARCHAR2(64);
    v_valtable          VARCHAR2(30);
    v_rowid             ROWID;
    i_cur               INTEGER;
    i_ret               INTEGER;
    i_count             INTEGER     :=0;
    i_commit            PLS_INTEGER :=0;
    g_geometry          MDSYS.SDO_GEOMETRY;
    v_ownertable        VARCHAR2(61);
    v_schema             VARCHAR2(30);
    v_table             VARCHAR2(30);
    v_geom              VARCHAR2(30);
  --
  BEGIN
    v_ownertable := GetOwnerObject( v_tablename );
    v_schema     := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table      := SplitOwnerObject( v_ownertable, 'TABLE');
    v_valtable   := SUBSTR( v_table, 1, 26 )|| c_valext;
    -- Check Inputs
    IF v_schema != USER THEN
      RAISE e_TabOwnerError;
    END IF;
    IF NOT ChkTable( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    IF NOT chkTable( v_valtable ) THEN
      Raise e_NoValTable;
    END IF;
    IF chkVIEW( v_ownertable ) THEN
      RAISE e_TableIsView;
    END IF;
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF NOT ChkGeometry( v_ownertable, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    --
    v_sql := 'SELECT SDO_ROWID FROM '|| v_valtable ||' WHERE SUBSTR(RESULT,1,5) IN (13349,13367,13356)';
    --
    i_cur := SYS.DBMS_SQL.OPEN_CURSOR;
    SYS.DBMS_SQL.PARSE( i_cur, v_sql, SYS.DBMS_SQL.NATIVE);
    SYS.DBMS_SQL.DEFINE_COLUMN_ROWID( i_cur, 1, v_rowid);
    i_ret := SYS.DBMS_SQL.EXECUTE( i_cur );
    LOOP
      IF( SYS.DBMS_SQL.FETCH_ROWS( i_cur ) = 0 ) THEN
        EXIT;
      END IF;
      i_count  := i_count + 1;
      i_commit := i_commit + 1;
      SYS.DBMS_SQL.COLUMN_VALUE_ROWID( i_cur, 1, v_rowid );
      v_debug:='ROWID: '|| v_rowid ||' in loop '|| i_count;
      v_sql:='SELECT SDO_UTIL.RECTIFY_GEOMETRY(A.'|| v_geom ||','|| n_tol ||') FROM '|| v_table ||' A WHERE ROWID=:vrowid'; 
      EXECUTE IMMEDIATE v_sql INTO g_geometry USING v_rowid;
      v_sql:='UPDATE '|| v_table ||' A SET A.'|| v_geom ||'=:geom where ROWID=:vrowid';
      EXECUTE IMMEDIATE v_sql USING g_geometry, v_rowid;
      IF i_commit = 500 THEN 
        COMMIT;
        i_commit := 1;
      END IF;
      COMMIT;
    END LOOP;
    SYS.DBMS_SQL.CLOSE_CURSOR( i_cur );
    Response( c_cmdname, c_feedback || i_count||' in '|| v_ownertable ||'.'|| v_geom);   
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_ownertable, c_msgGoomInternal);
    WHEN e_TableIsView THEN
      Response( c_cmdname || c_msgerror, c_msgTableIsView || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableIsView || v_ownertable, c_msgGoomInternal);
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgGeometryNotFound || v_geomcol);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgGeometryNotFound || v_geomcol, c_msgGoomInternal);
    WHEN e_TabOwnerError THEN
      Response( c_cmdname || c_msgerror, c_msgTabOwnerError || v_schema ||'<>'||USER);
      WRITE_RESULTS( v_schema, c_msgerror, c_cmdname, c_msgTabOwnerError || v_schema ||'!='||USER , c_msgGoomInternal);      
    WHEN e_NoValTable THEN
      RESPONSE( c_cmdname || c_msgError, c_msgNoValTable || v_table);
      RESPONSE( c_cmdname || c_msgInform,'Run EXEC GOOM.ValidateGeom('''|| v_table ||''','''|| v_geom ||''');');
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_table||'.'|| v_geom, v_sql, v_debug,SQLERRM);
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
  END RepairGeometry;
  --------------------------------------------------------------------------------------
  -- FixSmallArcs will find and stroke arcs (into line strings) that meet the chord length
  -- and tolerance criteria.  Small arcs can cause problems in both GeoMedia and in Oracle's 
  -- own spatial calculations.
  -- Syntax: EXEC GOOM.FixSmallArcs(c_table_name,c_geom_column,[n_chordlength],[n_tolerance]);
  --         n_chordlength : the length of the chord that measures the tolerance. Default 0.05.
  --         n_tol         : the distance between the arc and the chord length.  Default 0.001.
  -- Note: The defaults are representative of small arcs and work well.  Keep in mind that 
  -- these values are based on the units of storage.
  --
  PROCEDURE FixSmallArcs( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, 
                          v_length    IN NUMBER DEFAULT 0.05, n_tol IN NUMBER DEFAULT 0.001) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'FixSmallArcs';
    c_cmdtype       CONSTANT VARCHAR2(10) := 'VALIDATION';
    c_feedback      CONSTANT VARCHAR2(18) := 'Arcs Densified: ';
    --
    v_sql           VARCHAR2(1024);
    v_debug         VARCHAR2(1024);
    v_rowid         ROWID;
    g_geometry      MDSYS.SDO_GEOMETRY;
    i_cur           PLS_INTEGER;
    i_ret           PLS_INTEGER;
    i_count         PLS_INTEGER := 0;
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    v_geom          VARCHAR2(30);
    --
  BEGIN
    v_ownertable := GetOwnerObject( v_tablename );
    v_schema     := SplitOwnerObject( v_ownertable, 'OWNER');
    -- Check Inputs
    IF NOT ChkTable( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    IF chkVIEW( v_ownertable ) THEN
      RAISE e_TableIsView;
    END IF;
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_ownertable );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF NOT ChkGeometry( v_ownertable, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    --
    v_sql := 'SELECT ROWID FROM ' || v_ownertable || ' D where SDO_GEOM.SDO_LENGTH(D.' ||
              v_geom || ','|| c_tol ||'*10)<' || v_length;
    --
    i_cur := SYS.DBMS_SQL.OPEN_CURSOR;
    SYS.DBMS_SQL.PARSE( i_cur, v_sql, SYS.DBMS_SQL.NATIVE);
    SYS.DBMS_SQL.DEFINE_COLUMN_ROWID( i_cur, 1, v_rowid);
    i_ret := SYS.DBMS_SQL.EXECUTE( i_cur );
    LOOP
      IF (SYS.DBMS_SQL.FETCH_ROWS( i_cur ) = 0) THEN
        EXIT;
      END IF;
      i_count := i_count + 1;
      SYS.DBMS_SQL.COLUMN_VALUE_ROWID( i_cur, 1, v_rowid);
      v_sql := 'SELECT SDO_GEOM.SDO_ARC_DENSIFY(A.' || v_geom || ','||c_tol||'*10, ''arc_tolerance=' ||
                n_tol || ''') FROM ' || v_ownertable ||' A WHERE ROWID=''' || v_rowid || '''';
      EXECUTE IMMEDIATE v_sql INTO g_geometry;
      v_sql := 'UPDATE ' || v_ownertable || ' A SET A.' || v_geom || '=:geom where ROWID=''' || v_rowid || '''';
      EXECUTE IMMEDIATE v_sql USING g_geometry;
      COMMIT;
    END LOOP;
    SYS.DBMS_SQL.CLOSE_CURSOR( i_cur );
    Response( c_cmdname, c_feedback || i_count);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable || '.' || v_geom, c_feedback || i_count);
  EXCEPTION
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableNotFound || v_ownertable, c_msgGoomInternal);
    WHEN e_TableIsView THEN
      Response( c_cmdname || c_msgerror, c_msgTableIsView || v_ownertable);
      WRITE_RESULTS(v_schema, c_msgError, c_cmdname, c_msgTableIsView || v_ownertable, c_msgGoomInternal);
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgerror, c_msgGeometryNotFound || v_geomcol);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgGeometryNotFound || v_geomcol, c_msgGoomInternal);     
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geom, v_sql, v_debug, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geom, c_msgOraError || SQLCODE);
  END FixSmallArcs;
  --------------------------------------------------------------------------------------
  -- FixNullGeom repairs NULL (uninitialized) geometries caused by using SQL Loader.
  -- It does this by initializing the geometry column and making it empty.
  -- Syntax: EXEC GOOM.FixNullGeom; or EXEC GOOM.FixNullGeom([v_schema]);
  --         v_schema : optional, normally the current schema is used.  A DBA can specify 
  --                    any schema.
  -- Note: This procedure does not harm existing data.
  --
  PROCEDURE FixNullGeoms ( v_schema IN VARCHAR2 DEFAULT USER) IS
    c_cmdname       CONSTANT VARCHAR2(12) := 'FixNullGeoms';
    c_cmdtype       CONSTANT VARCHAR2(10) := 'VALIDATION';
    c_appliesto     CONSTANT VARCHAR2(19) := 'All Feature Classes';
    c_feedback      CONSTANT VARCHAR2(47) := 'Verified and repaired NULL geometry fields in: ';
    --
    v_feature       GetGeomBasedFCs%ROWTYPE;
    v_geometry      COLS.COLUMN_NAME%TYPE;
    v_tablename     COLS.TABLE_NAME%TYPE;
    v_ownertable    VARCHAR2(61);
    v_sql           VARCHAR2(255);
  BEGIN
    FOR v_feature IN GetGeomBasedFCs( v_schema ) LOOP
      v_tablename := v_feature.table_name;
      v_geometry  := v_feature.column_name;
      v_ownertable:= v_schema ||'.'|| v_tablename;
      IF chkVIEW( v_ownertable ) THEN
        Response( c_cmdname, c_msgTableIsView || v_ownertable);
      ELSE
        v_sql := 'UPDATE ' || v_ownertable || ' A SET A.' || v_geometry || '=NULL WHERE A.' || v_geometry || '.SDO_GTYPE IS NULL';
        EXECUTE IMMEDIATE v_sql;
      END IF;
    END LOOP;
    COMMIT;
    Response( c_cmdname, c_feedback || v_schema);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, c_appliesto, c_feedback || v_schema);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_geometry, v_sql, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable || '.' || v_geometry, c_msgOraError || SQLCODE);
  END FixNullGeoms;
  --------------------------------------------------------------------------------------
  -- FixTrainingSpaces removes trailing spaces from all VARCHAR2 columns in a schema.
  -- This is sometimes an issue when importing data from Microsoft Access based warehouses.
  -- Syntax: EXEC GOOM.FixTrailingSpaces;
  -- Note: This procedure does not harm existing data.
  --
  PROCEDURE FixTrailingSpaces ( v_schema IN VARCHAR2 DEFAULT USER) IS
  --
    CURSOR GetCharFields ( v_tabowner VARCHAR2 DEFAULT USER) IS
      SELECT TABLE_NAME, COLUMN_NAME
        FROM ALL_TAB_COLUMNS
       WHERE OWNER=v_tabowner
         AND DATA_TYPE = 'VARCHAR2'
         AND TABLE_NAME NOT LIKE 'BIN$%'
         AND TABLE_NAME NOT IN (SELECT VIEW_NAME FROM ALL_VIEWS WHERE OWNER = v_tabowner);
    --
    c_cmdname   CONSTANT VARCHAR2(32) := 'FixTrailingSpaces';
    c_cmdtype   CONSTANT VARCHAR2(10) := 'VALIDATION';
    v_feedback  CONSTANT VARCHAR2(29) := 'Character fields processed: ';
    --
    v_feature   GetCharFields%ROWTYPE;
    v_tablename COLS.COLUMN_NAME%TYPE;
    v_column    COLS.COLUMN_NAME%TYPE;
    v_sql       VARCHAR2(512);
    i_count     PLS_INTEGER := 0;
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
  BEGIN
    -- added for owner.table support
    v_ownertable := GetOwnerObject( v_tablename );
    --
    FOR v_feature IN GetCharFields( v_schema ) LOOP
      v_tablename := v_feature.table_name;
      v_ownertable:= v_schema||'.'||v_feature.table_name;
      v_column    := v_feature.column_name;
      v_sql       := 'UPDATE ' || v_ownertable || ' SET ' || v_column || '= RTRIM(' || v_column || ')';
      EXECUTE IMMEDIATE v_sql;
      COMMIT;
      i_count := i_count + 1;
    END LOOP;
    Response( c_cmdname, v_feedback || i_count);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable, v_feedback|| i_count);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_column, v_sql, v_feedback|| i_count, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable, c_msgOraError || SQLCODE);
  END FixTrailingSpaces;
  --------------------------------------------------------------------------------------
  -- AnalyzeGeometry looks at a single geometry value and tests it for validity.  It will
  -- return a detailed error message. If an error condition exists, a detailed analysis 
  -- will be returned to the console (if SERVEROUTPUT is ON).  If you only want the detailed
  -- analysis when an error condition exists, set the v_vebose flag to FALSE. the default 
  -- is TRUE so you will always get analysis.
  -- Syntax: SELECT GOOM.AnalyzeGeometry(g_geometry) FROM table WHERE PKID=val;
  --
  FUNCTION AnalyzeGeometry ( g_geometry IN SDO_GEOMETRY, v_verbose in VARCHAR2 DEFAULT 'TRUE') RETURN VARCHAR2 IS
    c_cmdname   CONSTANT VARCHAR2(32) := 'AnalyzeGeometry';
    c_cmdtype   CONSTANT VARCHAR2(10) := 'VALIDATION';             
    v_iGtype               NUMBER;
    v_sGType               VARCHAR2(4);
    v_sDim                 VARCHAR2(1);
    v_sType                VARCHAR2(1);
    v_sDimInfo             VARCHAR2(2);
    v_sTypeInfo            VARCHAR2(31);
    v_SRID                 VARCHAR2(31);
    v_SRID_Name            VARCHAR2(80);
    v_pt_flag              BOOLEAN;
    v_isOpt                BOOLEAN := FALSE;
    v_pt_Result            VARCHAR2(100);
    v_counter              NUMBER;
    v_par                  NUMBER;
    v_iTripletCounter      NUMBER;
    v_iSdo_Etype           NUMBER;
    v_sSdo_EtypeInfo       VARCHAR2(100);
    v_sSdo_Interpretation  VARCHAR2(255);
    v_iSdo_Interpretation  NUMBER;
    v_desc                 VARCHAR2(255);
    v_textflag             INTEGER := 0;
    v_textrotation         NUMBER;
    v_numtext              INTEGER := 0;
    v_ord                  INTEGER := 1;
    v_GeomChk              VARCHAR2(255);
    v_GeomInfo             VARCHAR2(255);
    v_debug                VARCHAR2(255);
    v_dbversion            VARCHAR2(12);
    v_sql                  VARCHAR2(128);
  --
  BEGIN
     -- Validate the geometry...
     -- Arc handling changed in 11G, now need new parameter introduced in 11.2.0.2 (flag10G).
     v_debug:='In geometry validation';
     v_dbversion:= substr(getdbversion,1,8);
     IF v_dbversion = '11.2.0.1' THEN
        v_sql:='SELECT SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT( :g_geometry, 0.00005, ''FALSE'') FROM DUAL';
     ELSE
        v_sql:='SELECT SDO_GEOM.VALIDATE_GEOMETRY_WITH_CONTEXT( :g_geometry, 0.00005, ''FALSE'',''TRUE'') FROM DUAL';
     END IF;
     EXECUTE IMMEDIATE v_sql INTO v_GeomChk using g_geometry;
     IF v_GeomChk = 'TRUE' THEN
       v_GeomInfo := 'Geometry passes Oracle data validation.';
     ELSE
       v_GeomInfo := 'ORA-'|| SUBSTR( v_GeomChk,1,5 ) ||': '||GetError(SUBSTR( v_GeomChk,1,5 ))||' Location:'||SUBSTR( v_GeomChk,6);
     END IF;
     -- End validation Check
     -- Report on geometry content to end user.  This is optional but is really purpose of this function.
     IF v_verbose <> 'FALSE' OR v_GeomChk <> 'TRUE' THEN
      v_debug:='Analyze SDO_GTYPE';
      v_iGtype := g_geometry.SDO_GTYPE;
      v_sGtype := TO_CHAR( v_iGtype );
      IF LENGTH( v_sGtype ) = 4 THEN
        v_sDim     := SUBSTR( v_sGtype,1,1 );
        v_sDimInfo := CASE v_sDim
          WHEN '2' THEN '2D'
          WHEN '3' THEN '3D'
          WHEN '4' THEN '4D'
          ELSE 'INVALID DIMENSION'
        END;
        v_sType     := SUBSTR( v_sGtype,4,1 );
        v_sTypeInfo := CASE v_sType
          WHEN '0' THEN 'UNKNOWN_GEOMETRY'
          WHEN '1' THEN 'POINT'
          WHEN '2' THEN 'LINESTRING'
          WHEN '3' THEN 'POLYGON'
          WHEN '4' THEN 'COLLECTION'
          WHEN '5' THEN 'MULTIPOINT'
          WHEN '6' THEN 'MULTILINESTRING'
          WHEN '7' THEN 'MULTIPOLYGON'
          ELSE 'INVALID GTYPE'
        END;
      END IF;
    --
      v_debug:='Analyze SDO_SRID';
      v_srid := TO_CHAR(g_geometry.sdo_srid);
      IF v_srid IS NULL THEN
        v_srid        := 'NULL';
        v_SRID_Name := 'No SRID Defined (projected data?)';
      ELSE
        SELECT CS_NAME INTO v_SRID_Name FROM MDSYS.CS_SRS WHERE SRID = v_srid;
      END IF;
    --
      v_debug:='Analyze SDO_POINT';
      IF g_geometry.SDO_POINT IS NULL THEN
        v_pt_flag   := FALSE;
        v_pt_Result := 'SDO_POINT is NULL: Oriented point format used.';
      ELSE
        v_pt_flag   := TRUE;
        v_pt_Result := 'SDO_POINT is not NULL: Native point format used.';
      END IF;
    --
    v_debug:='Main Output Section';
    TitleBlock('SDO_GEOMETRY Storage Analysis');
    Response('SDO_GTYPE', v_iGtype||' - '|| v_sDimInfo||' '|| v_sTypeInfo);
    Response('SDO_SRID', v_srid||' - '|| v_SRID_Name);
    IF v_pt_flag THEN
      Response('SDO_POINT', v_pt_Result);
      Response('->  X: ', g_geometry.sdo_point.x);
      Response('->  Y: ', g_geometry.sdo_point.y);
      Response('->  Z: ', g_geometry.sdo_point.z);
    ELSE
      Response('SDO_POINT: NULL ', v_pt_Result);
    END IF;
    --
    v_debug:='Elem Info Output';
    IF g_geometry.sdo_elem_info IS NULL then
      DotLine;
      Response('SDO_ELEM_INFO_ARRAY','NULL');
    ELSE
      DotLine;
      DBMSG('SDO_ELEM_INFO_ARRAY');
      v_iTripletCounter  :=  0;
      FOR v_counter in 1..g_geometry.sdo_elem_info.count LOOP
        v_iTripletCounter  :=  v_iTripletCounter + 1;
        IF v_iTripletCounter = 4 then
           v_iTripletCounter  :=  1;
        END IF;
        v_par := g_geometry.sdo_elem_info( v_counter );
        IF ( v_iTripletCounter = 1 ) THEN
          RESPONSE('-> '|| v_counter||': '|| v_par,'OFFSET: Index location in ordinate array');
        ELSIF (v_iTripletCounter = 2) THEN
          v_iSdo_Etype    :=  v_par;
          v_sSdo_EtypeInfo:=  CASE v_iSdo_Etype
                                WHEN    0 THEN 'User defined element type'
                                WHEN    1 THEN 'Point'
                                WHEN    2 THEN 'Line'
                                WHEN 1003 THEN 'Outer Polygon'
                                WHEN 2003 THEN 'Inner Polygon'
                                WHEN    4 THEN 'Compound Line'
                                WHEN 1005 THEN 'Outer Compound Polygon'
                                WHEN 2005 THEN 'Inner Compound Polygon'
                                ELSE '**INVALID SDO_ETYPE'
                              END;
          Response('-> '|| v_counter ||': '|| v_iSdo_Etype ,'SDO_ETYPE: '|| v_sSdo_EtypeInfo);
        ELSIF ( v_iTripletCounter = 3 ) THEN
          v_iSdo_Interpretation := v_par;
          v_debug:='In ETYPE Case';
          CASE v_iSdo_Etype
            WHEN    0 THEN
              IF    v_iSdo_Interpretation = 6000 THEN
                    v_sSdo_Interpretation  :=  'GeoMedia Oriented Point';
                    v_isOpt := TRUE;
              ELSIF v_iSdo_Interpretation = 6001 THEN
                    v_sSdo_Interpretation  :=  'GeoMedia Text';
                    v_textflag := 1;
              ELSIF v_iSdo_Interpretation = 6002 THEN
                    v_sSdo_Interpretation  :=  'GeoMedia Raster';
                    v_textflag := 2;
              ELSE  v_sSdo_Interpretation  :=  '** INVALID SDO_INTERPRETATION **';
              END IF;
            WHEN    1 THEN v_sSdo_Interpretation  :=  '# of points';
            WHEN    2 THEN
              IF    v_iSdo_Interpretation = 1 THEN v_sSdo_Interpretation  :=  'Straight line segments';
              ELSIF v_iSdo_Interpretation = 2 THEN v_sSdo_Interpretation  :=  'Circular arc segments';
              ELSE  v_sSdo_Interpretation  :=  '** INVALID SDO_INTERPRETATION **';
              END IF;
            WHEN 1003 THEN
              IF    v_iSdo_Interpretation = 1 THEN v_sSdo_Interpretation  :=  'Straight line segments';
              ELSIF v_iSdo_Interpretation = 2 THEN v_sSdo_Interpretation  :=  'Circular arc segments';
              ELSIF v_iSdo_Interpretation = 3 THEN v_sSdo_Interpretation  :=  'Rectangle';
              ELSIF v_iSdo_Interpretation = 4 THEN v_sSdo_Interpretation  :=  'Circle';
              ELSE  v_sSdo_Interpretation  :=  '** INVALID SDO_INTERPRETATION **';
              END IF;
            WHEN 2003 THEN
              IF    v_iSdo_Interpretation = 1 THEN v_sSdo_Interpretation  :=  'Straight line segments';
              ELSIF v_iSdo_Interpretation = 2 THEN v_sSdo_Interpretation  :=  'Circular arc segments';
              ELSIF v_iSdo_Interpretation = 3 THEN v_sSdo_Interpretation  :=  'Rectangle';
              ELSIF v_iSdo_Interpretation = 4 THEN v_sSdo_Interpretation  :=  'Circle';
              ELSE  v_sSdo_Interpretation  :=  '** INVALID SDO_INTERPRETATION **';
              END IF;
            WHEN    4 THEN v_sSdo_Interpretation  :=  '# of sub-elements (triplets)';
            WHEN 1005 THEN v_sSdo_Interpretation  :=  '# of sub-elements (triplets)';
            WHEN 2005 THEN v_sSdo_Interpretation  :=  '# of sub-elements (triplets)';
          END CASE;
          v_debug:='SDO_INTERPRETATION';
          IF LENGTH( v_iSdo_Interpretation )<4 THEN
            Response('-> '|| v_counter||': '|| v_iSdo_Interpretation,'SDO_INTERPRETATION: '|| v_sSdo_Interpretation);
          ELSE
            Response('-> '|| v_counter||': '|| v_iSdo_Interpretation,'SDO_INTERPRETATION: '|| v_sSdo_Interpretation);
          END IF;
        END IF;
      END LOOP;
    END IF;
    --
    -- SDO_ORDINATES Output
      IF g_geometry.sdo_ordinates IS NULL THEN
        DotLine;
        RESPONSE('SDO_ORDINATES_ARRAY','NULL');
      ELSE
        DotLine;
        DBMSG('SDO_ORDINATES_ARRAY');
        IF v_textflag = 0 THEN
          FOR v_counter IN 1..g_geometry.sdo_ordinates.count LOOP
            IF v_isOpt THEN
              IF v_counter=1    THEN v_desc  :=  'I Rotation Vector';
              ELSIF v_counter=2 THEN v_desc  :=  'J Rotation Vector';
              ELSIF v_counter=3 THEN v_desc  :=  'K Rotation Vector';
              ELSE  v_isOpt  :=  FALSE;
              END IF;
            ELSE
              CASE v_sDim
                 WHEN '2' THEN
                   IF v_ord =1 THEN
                     v_desc := 'X';
                     v_ord  :=  v_ord + 1;
                   ELSIF v_ord =2 THEN
                     v_desc := 'Y';
                     v_ord  :=  1;
                   END IF;
                 WHEN '3' THEN
                   IF v_ord =1 THEN
                     v_desc := 'X';
                     v_ord  :=  v_ord + 1;
                   ELSIF v_ord =2 THEN
                     v_desc := 'Y';
                     v_ord  :=  v_ord + 1;
                   ELSIF v_ord =3 THEN
                     v_desc := 'Z';
                     v_ord  :=  1;
                   END IF;
                 WHEN '4' THEN
                   IF v_ord =1 THEN
                     v_desc := 'X';
                     v_ord  :=  v_ord + 1;
                   ELSIF v_ord =2 THEN
                     v_desc := 'Y';
                     v_ord  :=  v_ord + 1;
                   ELSIF v_ord =3 THEN
                     v_desc := 'Z';
                     v_ord  :=  v_ord + 1;
                   ELSIF v_ord =4 THEN
                     v_desc := 'M';
                     v_ord  :=  1;
                   END IF;
              END CASE;
            END IF;
            v_par   :=  g_geometry.sdo_ordinates( v_counter );
            Response('-> '|| v_counter ||': '|| v_par , v_desc);
          END LOOP;
        ELSIF v_textflag = 1 THEN
          v_numtext := CEIL( g_geometry.sdo_ordinates(6)/4 );
          FOR v_counter IN 1..g_geometry.sdo_ordinates.count LOOP
            v_par := g_geometry.sdo_ordinates(v_counter);
            If v_counter=1 THEN 
               v_textrotation := ROUND( v_par ,2);
               IF v_textrotation > 360 THEN
                  v_desc := 'Text Rotation value too large ('|| v_textrotation ||' >360).';
               ELSE
                  v_desc := 'Text Rotation ('|| v_textrotation ||' degrees ).' ;
               END IF;        
            ELSIF v_counter=2 THEN v_desc := 'I Unit Vector';
            ELSIF v_counter=3 THEN v_desc := 'J Unit Vector';
            ELSIF v_counter=4 THEN v_desc := 'K Unit Vector';
            ELSIF v_counter=5 THEN v_desc := 'Format: '|| GetGMTextFormat( v_par );
            ELSIF v_counter=6 THEN v_desc := 'Number of bytes of text';
            ELSIF v_counter>6 and v_counter<=6+v_numtext THEN
             v_desc := 'Integer2Text -> '||Integer2Text( g_geometry.sdo_ordinates(v_counter) );
            ELSIF v_counter=6+v_numtext+1 THEN v_desc := 'X Ordinate';
            ELSIF v_counter=6+v_numtext+2 THEN v_desc := 'Y Ordinate';
            ELSIF v_counter=6+v_numtext+3 THEN v_desc := 'Z Ordinate';
            ELSE
              v_desc := NULL;
            END IF;
            Response('-> '||v_counter||': '|| v_par, v_desc);
          END LOOP;
        ELSE
          v_numtext := CEIL( g_geometry.sdo_ordinates(17)/2 );
          FOR v_counter IN 1..g_geometry.sdo_ordinates.count LOOP
            v_par := g_geometry.sdo_ordinates(v_counter);
            If v_counter = 1  THEN v_desc := 'Scale X';      
            ELSIF v_counter=4 THEN v_desc := 'Delta X';
            ELSIF v_counter=6 THEN v_desc := 'Scale Y';
            ELSIF v_counter=8 THEN v_desc := 'Delta Y';  
            ELSIF v_counter=11 THEN v_desc := 'Scale Y'; 
            ELSIF v_counter=12 THEN v_desc := 'Delta Z'; 
            ELSIF v_counter in (2,3,5,7,9,10,13,14,15,16) THEN v_desc := 'Matrix Padding'; 
            ELSIF v_counter=17 THEN v_desc := 'Length of Moniker'; 
            ELSIF v_counter>17 and v_counter <= 17 + v_numtext THEN
                  v_desc := 'Integer2Text -> '||Integer2Text( g_geometry.sdo_ordinates(v_counter) );
            ELSIF v_counter=17+ v_numtext +1 THEN v_desc := 'Raster Footprint - Standard 4 pt polygon:';
            ELSE
              v_desc := NULL;
            END IF;
            Response('-> '|| v_counter ||': '|| v_par, v_desc);
          END LOOP;
        END IF;
      END IF;
      DblLine;
    END IF;
    RETURN v_GeomInfo;
    EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_geomInfo, v_debug, SQLCODE, SQLERRM);
      RETURN 'Warning - Failure in Geometry Analysis Process';
  END AnalyzeGeometry;
  
  -- -----------------------------------------------------------------------------------------------------------------
  -- SPATIAL DATA FUNCTIONS AND PROCEDURES: Operates directly on geometry.
  -- -----------------------------------------------------------------------------------------------------------------

  -- GetPtCoord returns the ordinate associated with the specified type: X,Y, or Z.
  -- Syntax: n_coord:=GOOM.GetPtCoord(g_geom,v_type);
  --         SELECT GOOM.GetPtGeom(A.geometry,v_type) FROM table A;
  --         g_geometry : the actual geometry column (no quotes).
  --         v_typ      : 'X','Y', or 'Z' depending on what value you want.
  -- Note: Only point geometries can be used. 
  --
  FUNCTION GetPtCoord( g_geom IN MDSYS.SDO_GEOMETRY, v_typ IN VARCHAR2) RETURN NUMBER IS
    --
    c_cmdname       CONSTANT VARCHAR2(32):= 'GetPtCoord';
    c_feedback1     CONSTANT VARCHAR2(37):= 'Incorrect coordinate type specified: ';
    c_feedback2     CONSTANT VARCHAR2(30):= 'Geometry type is not POINT: ';
    c_feedback3     CONSTANT VARCHAR2(30):= 'Requested Z from 2D geometry: ';
    --
    v_GTYPE         VARCHAR2(4);
    i_dim           INTEGER;
    n_coord         FLOAT;
    v_pttype        VARCHAR2(32);
    v_type          VARCHAR2(1) := UPPER(v_typ);
    --
  BEGIN
    v_GTYPE := g_geom.SDO_GTYPE;
    i_dim   := SUBSTR(v_GTYPE, 1, 1);
    IF g_geom.SDO_POINT.X IS NULL THEN
      IF g_geom.SDO_ELEM_INFO(3) = 6000 AND g_geom.SDO_ELEM_INFO(2) = 0 THEN
        v_pttype := 'GEOMEDIA';
      ELSE
        v_pttype := 'ORACLE';
      END IF;
    ELSE
      v_pttype := 'NATIVE';
    END IF;
    IF v_GTYPE = '2001' OR v_GTYPE = '3001' THEN
      CASE v_type
        WHEN 'X' THEN
          CASE v_pttype
            WHEN 'NATIVE' THEN
              n_coord := g_geom.sdo_point.X;
            WHEN 'ORACLE' THEN
              n_coord := g_geom.sdo_ordinates(1);
            WHEN 'GEOMEDIA' THEN
              n_coord := g_geom.sdo_ordinates(4);
          END CASE;
          RETURN n_coord;
        WHEN 'Y' THEN
          CASE v_pttype
            WHEN 'NATIVE' THEN
              n_coord := g_geom.sdo_point.Y;
            WHEN 'ORACLE' THEN
              n_coord := g_geom.sdo_ordinates(2);
            WHEN 'GEOMEDIA' THEN
              n_coord := g_geom.sdo_ordinates(5);
          END CASE;
          RETURN n_coord;
        WHEN 'Z' THEN
          IF i_dim = 2
          THEN
            Response( c_cmdname || c_msgWarning, c_feedback3 || v_type);
            RETURN NULL;
          ELSE
            CASE v_pttype
              WHEN 'NATIVE' THEN
                n_coord := g_geom.sdo_point.Z;
              WHEN 'ORACLE' THEN
                n_coord := g_geom.sdo_ordinates(3);
              WHEN 'GEOMEDIA' THEN
                n_coord := g_geom.sdo_ordinates(6);
            END CASE;
            RETURN n_coord;
          END IF;
        ELSE
          Response( c_cmdname || c_msgWarning, c_feedback1 || v_type);
          RETURN NULL;
        END CASE;
      ELSE
          Response( c_cmdname || c_msgWarning, c_feedback2 || v_GTYPE);
      RETURN NULL;
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_type, v_pttype, SQLCODE, SQLERRM);
      RETURN NULL;
  END GetPtCoord;
  --------------------------------------------------------------------------------------
  -- GetLinearCoord returns the ordinate associated with the specified type: X,Y, or Z 
  -- from either the start or the end point of the line.
  -- n_coord:=GOOM.GetLinearCoord(g_geometry,v_typ,v_loc);
  -- SELECT GOOM.GetLinearCoord(A.geometry,'X','START') FROM table A;
  --        g_geometry : the actual geometry column (no quotes).
  --        v_typ      : 'X','Y', or 'Z' depending on what value you want.
  --        v_loc      : the location: 'START' or 'END'.
  -- 
  FUNCTION GetLinearCoord( g_geom IN MDSYS.SDO_GEOMETRY, v_typ IN VARCHAR2, v_loc IN VARCHAR2 DEFAULT 'START') RETURN NUMBER IS
    --
    c_cmdname       CONSTANT VARCHAR2(32) := 'GetLinearCoord';
    c_feedback1     CONSTANT VARCHAR2(37) := 'Incorrect coordinate type specified: ';
    c_feedback2     CONSTANT VARCHAR2(30) := 'Geometry type is not LINEAR: ';
    c_feedback3     CONSTANT VARCHAR2(30) := 'Requested Z from 2D geometry: ';
    --
    v_GTYPE     VARCHAR2(4);
    i_dim       INTEGER;
    n_coord     FLOAT;
    v_type      VARCHAR2(1) := UPPER(v_typ);
    v_location  VARCHAR2(5) := UPPER(v_loc);
    i_count     PLS_INTEGER;
    i_offsetX   PLS_INTEGER;
    i_offsetY   PLS_INTEGER;
    v_debug     VARCHAR2(30):='Init';
    --
    BEGIN
      v_GTYPE := g_geom.SDO_GTYPE;
      i_dim   := SUBSTR( v_GTYPE, 1, 1);
      i_count := g_geom.sdo_ordinates.count;
      CASE i_dim
       WHEN 2 THEN
         i_offsetX := i_count - 1;
         i_offsetY := i_count;
       WHEN 3 THEN
         i_offsetX := i_count - 2;
         i_offsetY := i_count - 1;
       END CASE;
      IF v_GTYPE IN ( '2002','3002','2006','3006' ) THEN
        CASE v_type
          WHEN 'X' THEN
            IF v_location = 'START' THEN
              n_coord := g_geom.sdo_ordinates(1);
            ELSIF v_location = 'END' THEN
              n_coord := g_geom.sdo_ordinates( i_offsetX );
            END IF;
            RETURN n_coord;
          WHEN 'Y' THEN
            IF v_location = 'START' THEN
              n_coord := g_geom.sdo_ordinates(2);
            ELSIF v_location = 'END' THEN
              n_coord := g_geom.sdo_ordinates( i_offsetY );
            END IF;
            RETURN n_coord;
          WHEN 'Z' THEN
            IF i_dim = 3 THEN
              IF v_location = 'START' THEN
                n_coord := g_geom.sdo_ordinates(2);
              ELSIF v_location = 'END' THEN
                n_coord := g_geom.sdo_ordinates( i_count );
              END IF;
              RETURN n_coord;
            ELSE
              RESPONSE( c_cmdname || c_msgWarning, c_feedback3);
              RETURN NULL;
            END IF;
          ELSE
            RESPONSE( c_msgWarning,c_feedback1 || v_type);
            RETURN NULL;
          END CASE;
        ELSE
          RESPONSE( c_msgWarning,c_feedback2 || v_GTYPE);
          RETURN NULL;
      END IF;
    EXCEPTION
      WHEN OTHERS THEN
        REPORT_ERROR( c_cmdname, 'Params:'|| v_type ||':'|| v_location, n_coord, v_debug, SQLERRM);
        RETURN NULL;
    END GetLinearCoord;
  --------------------------------------------------------------------------------------
  -- ConvPoly2Line takes an input polygon feature class and converts it to a linear 
  -- feature class.  The entire feature class is converted.  If you want to convert 
  -- just a geometry, use Oracle's SDO_UTIL.POLYGONTOLINE.
  -- Syntax: EXEC GOOM.ConvPoly2Line(v_tablename,v_geomcol);
  --         v_tablename : the table being modified.
  --         v_geomcol   : the column containing the geometry.
  -- Note: This command does not support owner.table.
  --
  PROCEDURE ConvPoly2Line( v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL) IS
    c_cmdname  CONSTANT VARCHAR2(32) := 'ConvPoly2Line';
    c_feedback CONSTANT VARCHAR2(28) := 'Polygon converted to lines: ';
    --
    v_sql      VARCHAR2(1024);
    v_debug    VARCHAR2(1024);
    v_rowid    ROWID;
    g_geometry MDSYS.SDO_GEOMETRY;
    i_cur      PLS_INTEGER;
    i_ret      PLS_INTEGER;
    v_geom     VARCHAR2(30);
  BEGIN
    IF v_geomcol IS NULL THEN
      v_geom := GETGEOM( v_tablename );
    ELSE
      v_geom := v_geomcol;
    END IF;
    IF NOT ChkTable( v_tablename ) THEN
      RAISE e_TableNotFound;
    END IF;
    IF NOT ChkGeometry( v_tablename, v_geom) OR v_geom IS NULL THEN
      RAISE e_GeometryNotFound;
    END IF;
    v_sql := 'SELECT ROWID FROM ' || v_tablename; -- Use the rowids to update the table
    --
    i_cur := SYS.DBMS_SQL.OPEN_CURSOR;
    SYS.DBMS_SQL.PARSE( i_cur, v_sql, SYS.DBMS_SQL.NATIVE);
    SYS.DBMS_SQL.DEFINE_COLUMN_ROWID( i_cur, 1, v_rowid);
    i_ret := SYS.DBMS_SQL.EXECUTE( i_cur );
    LOOP
      -- Loop thru table
      IF (SYS.DBMS_SQL.FETCH_ROWS( i_cur ) = 0) THEN
        -- exit when all rows retrieved
        EXIT;
      END IF;
      SYS.DBMS_SQL.COLUMN_VALUE_ROWID( i_cur, 1, v_rowid);
      v_sql := 'SELECT SDO_UTIL.POLYGONTOLINE(A.' || v_geom || ') from ' || v_tablename || ' A where ROWID=''' || v_rowid || '''';
      EXECUTE IMMEDIATE v_sql INTO g_geometry;
      v_sql := 'UPDATE ' || v_tablename || ' A SET A.' || v_geom || '=:geom where ROWID=''' || v_rowid || '''';
      EXECUTE IMMEDIATE v_sql USING g_geometry;
    END LOOP;
    SYS.DBMS_SQL.CLOSE_CURSOR( i_cur );
    Response( c_cmdname, c_feedback || v_tablename ||'.'|| v_geom);
  EXCEPTION
    WHEN e_GeometryNotFound THEN
      Response( c_cmdname || c_msgError, c_msgGeometryNotFound || v_tablename ||'.'|| v_geom);
    WHEN e_TableNotFound THEN
      Response( c_cmdname || c_msgError, c_msgTableNotFound || v_tablename);
    WHEN OTHERS THEN
      IF SYS.DBMS_SQL.IS_OPEN( i_cur ) THEN
         SYS.DBMS_SQL.CLOSE_CURSOR( i_cur);
      END IF;
      REPORT_ERROR( c_cmdname, v_tablename || '.' || v_geom, v_sql, v_debug, SQLERRM);
  END ConvPoly2Line;
  --------------------------------------------------------------------------------------
  -- ConvGeom2Pts accepts an input polygon or linear geometry and returns a point cluster
  -- geometry consisting of the vertices of the input line or polygon.
  -- Syntax: g_PtGeom:= GOOM.ConvGeom2Pts(g_geometry);
  --         UPDATE table A SET A.geometry = GOOM.ConvGeom2Pts(A.geometry);
  --         g_geometry : the actual geometry column (no quotes).
  --
  FUNCTION ConvGeom2Pts( g_polygeom in MDSYS.SDO_GEOMETRY) RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC IS
  c_cmdname       CONSTANT VARCHAR2(30):='Geom2PtCluster'; 
  -- 
  g_polygeometry  MDSYS.SDO_GEOMETRY;  
  v_ptgeom        MDSYS.SDO_GEOMETRY:=MDSYS.SDO_GEOMETRY(NULL,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(),MDSYS.SDO_ORDINATE_ARRAY());
  -- Other variables
  n_arctol        NUMBER:=0.1;    -- Arc Stroking must be minimum 20X geom tolerance.
  n_geomtol       NUMBER:=0.0005; -- Tolerance used in GeoMedia.
  i_gtype         INTEGER;        -- Geometry Type
  i_numpts        INTEGER;        -- Num of output points
  i_numords       INTEGER;        -- Num of input ordinates
  i_dim           INTEGER;        -- Dimension of geometry
  --
  BEGIN
    -- Process the input geometry
    -- First, get rid of any arcs so geometry is simple.
    g_polygeometry := SDO_GEOM.SDO_ARC_DENSIFY( g_polygeom, n_geomtol, 'arc_tolerance='|| n_arctol ||''''); 
    i_gtype        := g_polygeometry.SDO_GTYPE;                         -- Get the GTYPE
    i_dim          :=SUBSTR(TO_CHAR(i_gtype),1,1);                      -- Get the dimensions            
    IF i_gtype IN (2003,2007,3003,3007) THEN
      -- Convert polygon to linestring.  This helps in hole processing.
      g_polygeometry:=SDO_UTIL.PolygonToLine( g_polygeometry );
      i_numpts:=( g_polygeometry.sdo_ordinates.count / i_dim );
    ELSIF i_gtype IN (2002,2006,3002,3006) THEN
      -- Get the total number of points in line string
      i_numpts :=( g_polygeometry.sdo_ordinates.count / i_dim);      
    ELSE
      -- Wrong GTYPE, exit with input geometry
      RETURN g_polygeom; 
    END IF;                                  
    -- Create the output point cluster geometry.
    IF i_dim = 2 THEN
      v_ptgeom.SDO_GTYPE:=2005;                                  -- Two Dimensional Point Cluster             
    ELSE
      v_ptgeom.SDO_GTYPE:=3005;                                  -- Three Dimensional Point Cluster
    END IF;
    i_numords := g_polygeometry.sdo_ordinates.count;             -- Number of ordinates to process      
    v_ptgeom.SDO_ELEM_INFO.EXTEND(3);                            -- Initialize ELEM_INFO
    v_ptgeom.SDO_ELEM_INFO(1):=1;                                -- First Ordinate
    v_ptgeom.SDO_ELEM_INFO(2):=1;                                -- ETYPE for point
    v_ptgeom.SDO_ELEM_INFO(3):=i_numpts;                         -- Number points in cluster
    v_ptgeom.SDO_ORDINATES.EXTEND(i_numords);                    -- Initialize Ordinate Array by number of points
    FOR i IN 1..i_numords LOOP                                   -- Loops thru ordinates
     v_ptgeom.SDO_ORDINATES(i) := g_polygeometry.SDO_ORDINATES(i); -- Assign ordinates to new point geometry
    END LOOP;
    -- Remove duplicate points
    v_ptgeom:=SDO_UTIL.Remove_Duplicate_Vertices( v_ptgeom, n_geomtol);
    RETURN v_ptgeom;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, i_gtype, i_numpts,sqlcode,sqlerrm); 
      RETURN g_polygeom;
  END ConvGeom2Pts;
  --------------------------------------------------------------------------------------
  -- Convert2D returns a 2D geometry based on a 3D input geometry.  The Z is stripped off.
  -- Syntax: g_2DGeom:= GOOM.Convert2D(g_3dGeom);
  --         UPDATE table A SET A.geometry = GOOM.Convert2D(A.geometry);
  --         g_3dGeom : the actual geometry column (no quotes).     
  -- Note: This command does not support owner.table.  Raster and Text are not supported.
  --
  FUNCTION Convert2D( g_3DGeom IN MDSYS.SDO_GEOMETRY ) RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC AS
    c_cmdname   CONSTANT VARCHAR2(9) := 'Convert2D';
    --
    i_dim       PLS_INTEGER;
    i_srid      INTEGER := NULL;
    i_gdotype   PLS_INTEGER;
    i_gtype     PLS_INTEGER;
    i_elemcount PLS_INTEGER;
    i_elem      PLS_INTEGER;
    i_ordcount  PLS_INTEGER;
    i_count     PLS_INTEGER := 1;
    i_ord       PLS_INTEGER := 1;
    i_newOrd    PLS_INTEGER;
    g_TempGeom  MDSYS.SDO_GEOMETRY;
    v_debug     VARCHAR2(255);
  BEGIN
    v_debug   := 'Init';
    i_gdotype := SUBSTR( g_3DGeom.SDO_GTYPE, 4, 1);
    i_dim     := ( g_3DGeom.SDO_GTYPE - MOD( g_3DGeom.SDO_GTYPE, 1000)) / 1000;
    i_srid    := g_3DGeom.SDO_SRID;
    v_debug   := 'After Init';
    IF i_dim <> 3     THEN
      RAISE e_InvalidDimension;
    END IF;
    IF i_gdotype = 1 THEN
      --Point
      IF g_3DGeom.SDO_POINT.X IS NULL THEN
        v_debug := 'Oriented Point';
        IF g_3DGeom.SDO_ELEM_INFO(3) = 6000 THEN
          v_debug    := 'GM Oriented Point';
          g_TempGeom := MDSYS.SDO_GEOMETRY(2001, i_srid, NULL, MDSYS.SDO_ELEM_INFO_ARRAY(1, 0, 6000, 4, 1, 1), MDSYS.SDO_ORDINATE_ARRAY());
          g_TempGeom.SDO_ORDINATES.EXTEND(5);
          g_TempGeom.SDO_ORDINATES(1) := g_3DGeom.SDO_ORDINATES(1);
          g_TempGeom.SDO_ORDINATES(2) := g_3DGeom.SDO_ORDINATES(2);
          g_TempGeom.SDO_ORDINATES(3) := g_3DGeom.SDO_ORDINATES(3);
          g_TempGeom.SDO_ORDINATES(4) := g_3DGeom.SDO_ORDINATES(4);
          g_TempGeom.SDO_ORDINATES(5) := g_3DGeom.SDO_ORDINATES(5);
        ELSE
          v_debug    := 'Oracle Oriented Point';
          g_TempGeom := MDSYS.SDO_GEOMETRY(2001, i_srid, NULL, MDSYS.SDO_ELEM_INFO_ARRAY(1, 1, 1, 3, 1, 0), MDSYS.SDO_ORDINATE_ARRAY());
          g_TempGeom.SDO_ORDINATES.EXTEND(4);
          g_TempGeom.SDO_ORDINATES(1) := g_3DGeom.SDO_ORDINATES(1);
          g_TempGeom.SDO_ORDINATES(2) := g_3DGeom.SDO_ORDINATES(2);
          g_TempGeom.SDO_ORDINATES(3) := g_3DGeom.SDO_ORDINATES(4);
          g_TempGeom.SDO_ORDINATES(4) := g_3DGeom.SDO_ORDINATES(5);
        END IF;
      ELSE
        v_debug                := 'Native Point';
        g_TempGeom             := MDSYS.SDO_GEOMETRY(2001, i_srid, MDSYS.SDO_POINT_TYPE(0, 0, 0), NULL, NULL);
        v_debug                := 'Init Geom';
        g_TempGeom.SDO_POINT.X := g_3DGeom.SDO_POINT.X;
        g_TempGeom.SDO_POINT.Y := g_3DGeom.SDO_POINT.Y;
        g_TempGeom.SDO_POINT.Z := NULL;
      END IF;
      RETURN g_TempGeom;
    ELSIF i_gdotype = 2 OR i_gdotype = 6 OR i_gdotype = 3 OR i_gdotype = 7 THEN
      -- Line or Area
      CASE i_gdotype
        WHEN 2 THEN
          i_gtype := 2002; -- Simple Line
        WHEN 6 THEN
          i_gtype := 2006; -- Multiline
        WHEN 3 THEN
          i_gtype := 2003; -- Simple Area
        WHEN 7 THEN
          i_gtype := 2007; -- Multi Area
      END CASE;
      i_elemcount := g_3DGeom.SDO_ELEM_INFO.COUNT();
      i_ordcount  := g_3DGeom.SDO_ORDINATES.COUNT();
      g_TempGeom  := MDSYS.SDO_GEOMETRY( i_gtype, i_srid, NULL, MDSYS.SDO_ELEM_INFO_ARRAY(), MDSYS.SDO_ORDINATE_ARRAY());
      g_TempGeom.sdo_elem_info.EXTEND( i_elemcount );
      FOR I IN 1 .. i_elemcount / 3 LOOP
        i_elem := I * 3;
        i_newOrd := ((( g_3DGeom.SDO_ELEM_INFO( i_elem - 2 ) - 1) / 3) * 2) + 1;
        g_TempGeom.SDO_ELEM_INFO( i_elem - 2 ) := i_newOrd;
        g_TempGeom.SDO_ELEM_INFO( i_elem - 1 ) := g_3DGeom.SDO_ELEM_INFO( i_elem - 1);
        g_TempGeom.SDO_ELEM_INFO( i_elem)      := g_3DGeom.SDO_ELEM_INFO( i_elem) ;
      END LOOP;
      g_TempGeom.sdo_ordinates.EXTEND(( i_ordcount / 3) * 2 );
      FOR I IN 1 .. i_ordcount LOOP
        IF i_count = 3 THEN
          -- Ord is Z value so skip and reset
          i_count := 1;
        ELSE
          -- Ord is X or Y so update new ordinate array.
          g_TempGeom.SDO_ORDINATES( i_ord ) := g_3DGeom.SDO_ORDINATES(I);
          i_count := i_count + 1; -- Increase counter for Z Check
          i_ord := i_ord + 1;     -- Increase output array value.
        END IF;
      END LOOP;
      RETURN g_TempGeom;
    ELSE
      Response( c_cmdname || c_msgError, c_msgInvalidGType || i_gtype);
      RETURN g_3DGeom; -- Return Input Geom
    END IF;
  EXCEPTION
    WHEN e_InvalidDimension THEN
      Response( c_cmdname || c_msgError, c_msgInvalidDimension);
      RETURN g_3DGeom;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, i_gdotype, v_debug, SQLCODE, SQLERRM);
      RETURN g_3DGeom;
  END Convert2D;
  --------------------------------------------------------------------------------------
  -- Convert3D returns a 3D geometry based on a 2D input geometry.  The Z is populated by 
  -- the specified Z value.  Only a constant Z can be used and the default is 0.
  -- Syntax: g_3DGeom:= GOOM.Convert3D(g_2dGeom);
  --         UPDATE table A SET A.geometry = GOOM.Convert3D(A.geometry);
  --         g_2dGeom : the actual geometry column (no quotes). 
  --         n_Zval   : populates the missing Z value. Default = 0.
  -- Note: This command does not support owner.table.  Raster and Text are not supported.
  --
  FUNCTION Convert3D( g_2DGeom IN MDSYS.SDO_GEOMETRY, n_Zval In NUMBER Default 0) RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC AS
    c_cmdname       CONSTANT VARCHAR2(9)  := 'Convert3D';
    --  
    i_dim           PLS_INTEGER;
    i_srid          INTEGER := NULL;
    i_gdotype       PLS_INTEGER;
    i_gtype         PLS_INTEGER;
    i_elemcount     PLS_INTEGER;
    i_elem          PLS_INTEGER;
    i_ordcount      PLS_INTEGER;
    i_count         PLS_INTEGER := 1;
    i_zcount        PLS_INTEGER := 0;
    i_newOrd        PLS_INTEGER;
    g_TempGeom      MDSYS.SDO_GEOMETRY;
    v_debug         VARCHAR2(255);
  BEGIN
    i_gdotype := SUBSTR( g_2DGeom.SDO_GTYPE, 4, 1);
    i_dim     := ( g_2DGeom.SDO_GTYPE - MOD( g_2DGeom.SDO_GTYPE, 1000)) / 1000;
    i_srid    := g_2DGeom.SDO_SRID;
    IF i_dim != 2 THEN
      RAISE e_InvalidDimension;
    END IF;
  	IF i_gdotype = 1 THEN
  		--Point
  		IF g_2DGeom.SDO_POINT.X IS NULL THEN
  			v_debug := 'Oriented Point';
  			IF g_2DGeom.SDO_ELEM_INFO(3) = 6000 THEN
  			    v_debug    := 'GM Oriented Point';
  			    g_TempGeom := MDSYS.SDO_GEOMETRY(3001, i_srid, NULL, MDSYS.SDO_ELEM_INFO_ARRAY(1, 0, 6000, 4, 1, 1), MDSYS.SDO_ORDINATE_ARRAY());
  			    g_TempGeom.SDO_ORDINATES.EXTEND(6);
  			    g_TempGeom.SDO_ORDINATES(1) := g_2DGeom.SDO_ORDINATES(1);
  			    g_TempGeom.SDO_ORDINATES(2) := g_2DGeom.SDO_ORDINATES(2);
  			    g_TempGeom.SDO_ORDINATES(3) := g_2DGeom.SDO_ORDINATES(3);
  			    g_TempGeom.SDO_ORDINATES(4) := g_2DGeom.SDO_ORDINATES(4);
  			    g_TempGeom.SDO_ORDINATES(5) := g_2DGeom.SDO_ORDINATES(5);
  			    g_TempGeom.SDO_ORDINATES(6) := n_Zval;
  			ELSE
  		        v_debug    := 'Oracle Oriented Point';
  			    g_TempGeom := MDSYS.SDO_GEOMETRY(3001, i_srid, NULL, MDSYS.SDO_ELEM_INFO_ARRAY(1, 1, 1, 4, 1, 0), MDSYS.SDO_ORDINATE_ARRAY());
  		        g_TempGeom.SDO_ORDINATES.EXTEND(6);
  			    g_TempGeom.SDO_ORDINATES(1) := g_2DGeom.SDO_ORDINATES(1);
  			    g_TempGeom.SDO_ORDINATES(2) := g_2DGeom.SDO_ORDINATES(2);
  			    g_TempGeom.SDO_ORDINATES(3) := n_Zval;
  			    g_TempGeom.SDO_ORDINATES(4) := g_2DGeom.SDO_ORDINATES(3);
  			    g_TempGeom.SDO_ORDINATES(5) := g_2DGeom.SDO_ORDINATES(4);
  			    g_TempGeom.SDO_ORDINATES(6) := 0;
  			END IF;
  		ELSE
  		    v_debug                := 'Native Point';
  		    g_TempGeom             := MDSYS.SDO_GEOMETRY(3001, i_srid, MDSYS.SDO_POINT_TYPE(0, 0, 0), NULL, NULL);
  		    v_debug                := 'Init Geom';
  		    g_TempGeom.SDO_POINT.X := g_2DGeom.SDO_POINT.X;
  		    g_TempGeom.SDO_POINT.Y := g_2DGeom.SDO_POINT.Y;
  		    g_TempGeom.SDO_POINT.Z := n_Zval;
  		END IF;
  		RETURN g_TempGeom;
  	ELSIF i_gdotype = 2 OR i_gdotype = 6 OR i_gdotype = 3 OR i_gdotype = 7 THEN
  		-- Line or Area
  		CASE i_gdotype
  			WHEN 2 THEN
  			    i_gtype := 3002; -- Simple Line
  			WHEN 6 THEN
  			    i_gtype := 3006; -- Multiline
  			WHEN 3 THEN
  			    i_gtype := 3003; -- Simple Area
  			WHEN 7 THEN
  			    i_gtype := 3007; -- Multi Area
  		END CASE;
  	    i_elemcount := g_2DGeom.SDO_ELEM_INFO.COUNT();
  	    i_ordcount  := g_2DGeom.SDO_ORDINATES.COUNT();
  	    g_TempGeom  := MDSYS.SDO_GEOMETRY(i_gtype, i_srid, NULL, MDSYS.SDO_ELEM_INFO_ARRAY(), MDSYS.SDO_ORDINATE_ARRAY());
  		-- Re-assign the element info array adjusting the ordinate count as necessary.
  	    g_TempGeom.sdo_elem_info.EXTEND(i_elemcount);       -- Extend the array 
  		FOR I IN 1 .. i_elemcount / 3 LOOP                  -- Loop based on triplet
  	        i_elem := I * 3;                                -- Handle triplet backwards
  			If g_2DGeom.SDO_ELEM_INFO( i_elem - 2 ) = 1 Then  -- 1st element of triplet
  			   i_newOrd := 1;                              -- If its 1 it needs to stay 1
  			Else
  			    i_newOrd := TRUNC(( g_2DGeom.SDO_ELEM_INFO( i_elem - 2 ) / 2) *3);  -- Otherwise increase by 1/3 to cover z's
  			End If;
  		    g_TempGeom.SDO_ELEM_INFO( i_elem - 2 ) := i_newOrd;
  		    g_TempGeom.SDO_ELEM_INFO( i_elem - 1 ) := g_2DGeom.SDO_ELEM_INFO( i_elem - 1);
  		    g_TempGeom.SDO_ELEM_INFO( i_elem )     := g_2DGeom.SDO_ELEM_INFO( i_elem );
  		END LOOP;
  		-- Reassign sdo_ordinate array values
        g_TempGeom.sdo_ordinates.EXTEND(( i_ordcount / 2) * 3);  -- Extend the array by 1/3 to include Z
  		FOR I IN 1 .. (( i_ordcount / 2) * 3) LOOP               -- Cycle thru new array
			If i_count = 3 Then                                 -- Every 3rd ordinate is Z
			    g_TempGeom.SDO_ORDINATES(I) := n_Zval;          -- Assign the Z
		        i_count                     := 1;               -- then reset count for next Z
		        i_zcount                    := i_zcount + 1;    -- Keep track of total Zs assigned
			Else
			    -- Assign existing value for X and Y then adjust ordinate by number of Zs already assigned.
		        g_TempGeom.SDO_ORDINATES(I) := g_2DGeom.SDO_ORDINATES(I - i_zcount);
			    i_count                     := i_count + 1;   -- Increase counter for Z value
			END IF;
  		END LOOP;
  		RETURN g_TempGeom;
  	ELSE
  		Response( c_cmdname || c_msgError, c_msgInvalidGType || i_gdotype);
  		RETURN g_2DGeom; -- Return Input Geom
  	END IF;
  EXCEPTION
    WHEN e_InvalidDimension THEN
      Response( c_cmdname || c_msgError, c_msgInvalidDimension);
      RETURN g_2DGeom;
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, i_gdotype, v_debug, SQLCODE, SQLERRM);
      RETURN g_2DGeom;
  END Convert3D;
  --------------------------------------------------------------------------------------
  -- Convert2NativePt returns a 2D/3D native point geometry from an input 2D/3D oriented point geometry. 
  -- Syntax: g_geom:= GOOM.Convert2NativePt(g_PtGeom,[i_dim]);
  --         UPDATE table A SET A.geometry = GOOM.Convert2NativePt(A.geometry);
  --         g_geom : the actual geometry column (no quotes). 
  --         i_dim  : the optional output dimension (2 or 3). Default=2. 
  --
  FUNCTION Convert2NativePt ( g_geom  IN MDSYS.SDO_GEOMETRY, i_dim IN INTEGER DEFAULT 2) RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC AS
   c_cmdname        CONSTANT VARCHAR2(16):='Convert2NativePt'; 
   -- Init Vars
   v_npoint          MDSYS.SDO_GEOMETRY := NULL ;
   i_gtype           INTEGER := 2001;
   i_srid            INTEGER := NULL;
   n_Xpt             NUMBER  := NULL;
   n_Ypt             NUMBER  := NULL;
   n_Zpt             NUMBER  := NULL;
   v_debug           VARCHAR2(30):='Init';
   e_AlreadyNativePT EXCEPTION;
  BEGIN
    -- Collect pass through parameters from input geometry.
    i_srid  := g_geom.SDO_SRID;
    i_gtype := g_geom.SDO_GTYPE;
    IF i_gtype NOT IN (2001,3001) THEN         -- Check to make sure input geometry is a point geometry.
      RAISE e_InvalidGType;                    -- if not raise an exception.
    END IF;
    IF g_geom.SDO_POINT.X IS NOT NULL THEN     -- If input is already a native point, 
      RAISE e_AlreadyNativePT;                 -- raise exception and return input geom
    END IF;
    -- Check and set dimensions for output native points.
    IF i_dim = 3 and i_gtype > 2001 then       -- Input 3D - Output 2D
      n_Xpt   := g_geom.SDO_ORDINATES(1);
      n_Ypt   := g_geom.SDO_ORDINATES(2);
      n_Zpt   := g_geom.SDO_ORDINATES(3);
      i_gtype := 3001;                         
    ELSIF i_dim = 3 and i_gtype < 3001 then    -- Input 2D - Output 3D
      n_Xpt   := g_geom.SDO_ORDINATES(1);
      n_Ypt   := g_geom.SDO_ORDINATES(2);
      n_Zpt   := 0;
      i_gtype := 3001;
    ELSE                                       -- Input 2D - Output 2D
      n_Xpt   := g_geom.SDO_ORDINATES(1);
      n_Ypt   := g_geom.SDO_ORDINATES(2);
      n_Zpt   := NULL;
      i_gtype := 2001;
    END IF;
    -- Build and return a native point geometry.
    v_npoint := SDO_GEOMETRY( i_gtype, i_srid, SDO_POINT_TYPE( n_Xpt, n_Ypt, n_Zpt), NULL, NULL);
    RETURN v_npoint;
    -- Handle exception.
  EXCEPTION
    WHEN e_AlreadyNativePT THEN                -- Already a native point, return input geom.
      RETURN g_geom;
    WHEN e_InvalidGType THEN                   -- Bad GTYPE, return original data.
      Response( c_cmdname || c_msgError, c_msgInvalidGType || i_gtype);
      RETURN g_geom;
    WHEN OTHERS THEN                           -- General Error, return original data.
      REPORT_ERROR( c_cmdname, i_gtype, v_debug,SQLCODE,SQLERRM);
      RETURN g_geom;
  END Convert2NativePt;
  --------------------------------------------------------------------------------------
  -- Convert2OrientedPt returns a 2D/3D oriented point geometry from an input 2D/3D native pt geometry.
  -- The orientation matrix is set to 0.
  -- Syntax: g_geom:= GOOM.Convert2OrientedPt(g_PtGeom);
  --         UPDATE table A SET A.geometry = GOOM.Convert2OrientedPt(A.geometry);
  --         g_geom : the actual geometry column (no quotes). 
  --
  FUNCTION Convert2OrientedPt ( g_geom  IN MDSYS.SDO_GEOMETRY) RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC AS
   c_cmdname        CONSTANT VARCHAR2(18):='Convert2OrientedPt'; 
   -- Init Vars
   g_ptgeom          MDSYS.SDO_GEOMETRY := NULL ;
   i_gtype           INTEGER := NULL;
   i_srid            INTEGER := NULL;
   n_xcoord          NUMBER  := NULL;
   n_ycoord          NUMBER  := NULL;
   n_zcoord          NUMBER  := NULL;
   n_irot            NUMBER  := 0;
   n_jrot            NUMBER  := 0;
   n_krot            NUMBER  := 0;
   i_dim             PLS_INTEGER;
   v_debug           VARCHAR2(30):='Init';
   e_invalidPtGeom   EXCEPTION;
  BEGIN
    -- Collect parameters from input geometry.
    i_srid  := g_geom.SDO_SRID;
    i_gtype := g_geom.SDO_GTYPE;
    i_dim   := SUBSTR( i_gtype, 1, 1);
    v_debug := 'Params';
    IF i_gtype NOT IN (2001,3001) THEN         -- Check to make sure input geometry is a point geometry.
      RAISE e_InvalidGType;                    -- if not raise an exception.
    END IF;
    IF g_geom.SDO_ELEM_INFO(3) <> 6000 AND g_geom.SDO_POINT.X IS NULL THEN    -- Check to make sure input geometry is a GM Point.
      RAISE e_InvalidPtGeom;                                                  -- if not raise an exception.
    END IF;
    v_debug := 'Checks';
    IF g_geom.SDO_POINT.X IS NOT NULL THEN     -- Input is Native, 
      CASE i_dim
       WHEN 2 THEN
        n_xcoord := g_geom.sdo_point.X;
        n_ycoord := g_geom.sdo_point.Y;
        n_irot   := 1;
        n_jrot   := 0;
       WHEN 3 THEN
        n_xcoord := g_geom.sdo_point.X;
        n_ycoord := g_geom.sdo_point.Y;
        n_zcoord := g_geom.sdo_point.Z;
        n_irot   := 1;
        n_jrot   := 0;
        n_krot   := 0;
       ELSE
        RAISE e_InvalidDimension;
      END CASE;                 
    ELSE
      CASE i_dim
       WHEN 2 THEN
        n_irot  := g_geom.SDO_ORDINATES(1);
        n_jrot  := g_geom.SDO_ORDINATES(2);
        n_krot  := g_geom.SDO_ORDINATES(3);
        n_xcoord:= g_geom.SDO_ORDINATES(4);
        n_ycoord:= g_geom.SDO_ORDINATES(5);
       WHEN 3 THEN
        n_irot  := g_geom.SDO_ORDINATES(1);
        n_jrot  := g_geom.SDO_ORDINATES(2);
        n_krot  := g_geom.SDO_ORDINATES(3);
        n_xcoord:= g_geom.SDO_ORDINATES(4);
        n_ycoord:= g_geom.SDO_ORDINATES(5);
        n_zcoord:= g_geom.SDO_ORDINATES(6);
       ELSE
        RAISE e_InvalidDimension;
      END CASE;                 
    END IF;
    v_debug := 'Assignments';
    -- Construct new PT GEOM
    CASE i_dim
     WHEN 2 THEN
      g_ptgeom := SDO_GEOMETRY(2001, i_srid, NULL,SDO_ELEM_INFO_ARRAY(1,1,1,3,1,0),MDSYS.SDO_ORDINATE_ARRAY());
      g_ptgeom.SDO_ORDINATES.extend(4);
      g_ptgeom.SDO_ORDINATES(1) := n_xcoord;
      g_ptgeom.SDO_ORDINATES(2) := n_ycoord;
      g_ptgeom.SDO_ORDINATES(3) := n_irot;
      g_ptgeom.SDO_ORDINATES(4) := n_jrot;
     WHEN 3 THEN
      g_ptgeom := SDO_GEOMETRY(3001, i_srid, NULL,SDO_ELEM_INFO_ARRAY(1,1,1,4,1,0),MDSYS.SDO_ORDINATE_ARRAY());
      g_ptgeom.SDO_ORDINATES.extend(6);
      g_ptgeom.SDO_ORDINATES(1) := n_xcoord;
      g_ptgeom.SDO_ORDINATES(2) := n_ycoord;
      g_ptgeom.SDO_ORDINATES(3) := n_zcoord;
      g_ptgeom.SDO_ORDINATES(4) := n_irot;
      g_ptgeom.SDO_ORDINATES(5) := n_jrot;
      g_ptgeom.SDO_ORDINATES(6) := n_krot;
     ELSE
      RAISE e_InvalidDimension;
    END CASE;
    v_debug := 'Geometry';                 
    RETURN g_ptgeom;
    -- Build and return a native point geometry.
    -- Handle exception.
  EXCEPTION
    WHEN e_InvalidGType THEN              -- Bad GTYPE, return input geom.
      Response( c_cmdname || c_msgError, c_msgInvalidGType || i_gtype);
      RETURN g_geom;
    WHEN e_InvalidPtGeom THEN             -- Bad PT, return input geom.
      RETURN g_geom;
    WHEN e_InvalidDimension THEN              -- Bad Dimension, return input geom.
      Response( c_cmdname || c_msgError, c_msgInvalidDimension || i_gtype);
      RETURN g_geom;
    WHEN OTHERS THEN                      -- General Error, return input geom.
      REPORT_ERROR( c_cmdname, i_gtype, v_debug, SQLCODE, SQLERRM);
      RETURN g_geom;
  END Convert2OrientedPt; 
  --------------------------------------------------------------------------------------
  -- GetPtBearing returns the bearing (in radians or degrees) between input point 1 and input point 2.
  -- Syntax: n_bearing:= GOOM.GetPtBearing(g_ptgeom1,g_ptgeom1, v_rtype);
  --         g_ptgeom1 and g_ptgeom2 are actual geometries, no quotes.
  --         v_rtype : determines result in degrees or radians.  D (degrees) is the default.
  --
  FUNCTION GetPtBearing( g_geom1 IN SDO_GEOMETRY, g_geom2 in SDO_GEOMETRY, v_rtype IN VARCHAR2 DEFAULT 'D') RETURN NUMBER is 
    c_cmdname   VARCHAR2(12) := 'GetPtBearing';
    n_xcoord1   NUMBER;
    n_ycoord1   NUMBER;
    n_xcoord2   NUMBER;
    n_ycoord2   NUMBER;
    i_srid1     INTEGER :=0;
    i_srid2     INTEGER :=0;
    --
  BEGIN
    i_srid1 := NVL( g_geom1.SDO_SRID, 0);
    i_srid2 := NVL( g_geom2.SDO_SRID, 0);
    IF i_srid1 <> i_srid2 THEN
      RAISE e_DifferentSRID;
    END IF;
    IF SUBSTR( g_geom1.SDO_GTYPE, 4, 1) = 1 AND SUBSTR( g_geom2.SDO_GTYPE, 4, 1) = 1 THEN
      IF g_geom1.SDO_ELEM_INFO(3) = 6000 THEN
        n_xcoord1 := g_geom1.SDO_ORDINATES(4);
        n_ycoord1 := g_geom1.SDO_ORDINATES(5);
      ELSE
        n_xcoord1 := g_geom1.SDO_ORDINATES(1);
        n_ycoord1 := g_geom1.SDO_ORDINATES(2);
      END IF;
      IF g_geom2.SDO_ELEM_INFO(3) = 6000 THEN
        n_xcoord2 := g_geom2.SDO_ORDINATES(4);
        n_ycoord2 := g_geom2.SDO_ORDINATES(5);
      ELSE
        n_xcoord2 := g_geom2.SDO_ORDINATES(1);
        n_ycoord2 := g_geom2.SDO_ORDINATES(2);
      END IF;
    ELSE
      RAISE e_InvalidGType;     
    END IF;
    IF isGeographic( i_srid1 ) THEN
      RETURN Bearing( n_xcoord1, n_ycoord1, n_xcoord2, n_ycoord2, 'LL', v_rtype);
    ELSIF NOT isGeographic( i_srid1 ) THEN
      RETURN Bearing( n_xcoord1, n_ycoord1, n_xcoord2, n_ycoord2, 'P', v_rtype);
    ELSE
      RAISE e_invalidSRID;
    END IF;
  EXCEPTION
    WHEN e_InvalidGType THEN
      REPORT_ERROR( c_cmdname, g_geom1.SDO_GTYPE ||':'|| g_geom2.SDO_GTYPE, c_msgInvalidGtype, SQLCODE, SQLERRM);
    WHEN e_InvalidSRID THEN
      REPORT_ERROR( c_cmdname,'SRID1:'|| i_srid1 ||' SRID2:'|| i_srid2, c_msgInvalidSRID, SQLCODE,SQLERRM);
    WHEN e_DifferentSRID THEN
      REPORT_ERROR( c_cmdname,'SRID1:'|| i_srid1 ||' SRID2:'|| i_srid2, c_msgDifferentSRID, SQLCODE,SQLERRM);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname,'General Error', c_msgOraError,SQLCODE,SQLERRM);
  END GetPtBearing;
  --------------------------------------------------------------------------------------
  -- GetLinearBearing returns the bearing (in radians or degrees) between the start point
  -- and end point of the input linear geometry.
  -- Syntax: n_bearing:= GOOM.GetLinearBearing(g_geom,v_rtype);
  --         Select GOOM.GetLinearBearing(<GEOMETRY>, v_rtype) FROM DUAL;
  --         g_geom  : the actual geometry column (no quotes).
  --         v_rtype : determines result in degrees or radians.  D (degrees) is the default.
  --
  FUNCTION GetLinearBearing( g_geom IN SDO_GEOMETRY, v_rtype IN VARCHAR DEFAULT 'D') RETURN NUMBER is
    c_cmdname   VARCHAR2(16) := 'GetLinearBearing';
    --
    n_xcoord1   NUMBER;
    n_ycoord1   NUMBER;
    n_xcoord2   NUMBER;
    n_ycoord2   NUMBER;
    i_srid      INTEGER;
  BEGIN
    i_srid := NVL( g_geom.SDO_SRID, 0);
    IF SUBSTR( g_geom.SDO_GTYPE, 4, 1) = 2 THEN
      n_xcoord1 := GetLinearCoord( g_geom,'X','START');
      n_ycoord1 := GetLinearCoord( g_geom,'Y','START');
      n_xcoord2 := GetLinearCoord( g_geom,'X','END');
      n_ycoord2 := GetLinearCoord( g_geom,'Y','END');
    ELSE
      RAISE e_InvalidGtype;     
    END IF;
    IF isGeographic( i_srid ) THEN
      RETURN Bearing( n_xcoord1, n_ycoord1, n_xcoord2, n_ycoord2, 'LL', v_rtype);
    ELSIF NOT isGeographic( i_srid ) THEN
      RETURN Bearing( n_xcoord1, n_ycoord1, n_xcoord2, n_ycoord2, 'P', v_rtype);
    ELSE
      RAISE e_invalidSRID;
    END IF;
  EXCEPTION
    WHEN e_InvalidGType THEN
      REPORT_ERROR( c_cmdname, g_geom.SDO_GTYPE, c_msgInvalidGtype,SQLCODE,SQLERRM);
    WHEN e_InvalidSRID THEN
      REPORT_ERROR( c_cmdname,'SRID:'|| i_srid, c_msgInvalidSRID,SQLCODE,SQLERRM);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname,'General Error', c_msgOraError,SQLCODE,SQLERRM);
  END GetLinearBearing;
  --------------------------------------------------------------------------------------
  -- GetPointRotation returns the rotation (in radians or degrees) of an oriented point.
  -- Syntax: n_rotation:= GOOM.GetPointRotation(g_geom,v_type);
  --         Select GOOM.GetPointRotation(A.<GEOMETRY>, v_type) FROM table A;   
  --         g_geom : the actual geometry column (no quotes).
  --         v_type : determines result in degrees or radians.  D (degrees) is the default.
  --
  FUNCTION GetPointRotation( g_geom IN SDO_GEOMETRY, c_type in VARCHAR2 DEFAULT 'D') RETURN NUMBER IS
     c_cmdname         VARCHAR2(32):='GetPointRotation';
     i_gtype           INTEGER := NULL;
     n_irot            NUMBER  := 0;
     n_jrot            NUMBER  := 0;
     n_krot            NUMBER  := 0;
     i_dim             PLS_INTEGER;
     i_eleminfo        PLS_INTEGER;
     v_debug           VARCHAR2(30):='Init';
     --
     e_InvalidPtGeom   EXCEPTION;
  BEGIN
      i_gtype          := g_geom.SDO_GTYPE;
      i_dim            := SUBSTR( i_gtype, 1, 1);
      i_eleminfo       := g_geom.SDO_ELEM_INFO(3);
      IF i_gtype NOT IN (2001,3001) THEN         -- Check to make sure input geometry is a point geometry.
        RAISE e_InvalidGType;                    -- if not raise an exception.
      END IF;
      IF g_geom.SDO_POINT.X IS NOT NULL THEN     -- Input is Native, 
       RETURN 0;                                 -- No Rotation
      ELSIF i_eleminfo = 6000 THEN               -- GMP 9i oriented pt
          n_irot  := g_geom.SDO_ORDINATES(1);
          n_jrot  := g_geom.SDO_ORDINATES(2);
          n_krot  := g_geom.SDO_ORDINATES(3);
      ELSE                                       -- Oracle Native Oriented pt
        CASE i_dim
         WHEN 2 THEN
          n_irot  := g_geom.SDO_ORDINATES(3);
          n_jrot  := g_geom.SDO_ORDINATES(4);
          n_krot  := 0;
         WHEN 3 THEN
          n_irot  := g_geom.SDO_ORDINATES(4);
          n_jrot  := g_geom.SDO_ORDINATES(5);
          n_krot  := g_geom.SDO_ORDINATES(6);
         ELSE
          RAISE e_InvalidDimension;
        END CASE;                     
      END IF;  
      -- Calculate rotation from I and J values   ATAN2(y,x) or ATAN2(j,i) 
      IF c_type = 'D' THEN                     -- return degrees
        RETURN ATAN2( n_jrot, n_irot) * (180/PI);
      ELSE                                     -- return radians
        RETURN ATAN2( n_jrot, n_irot);
      END IF;            
    EXCEPTION
      WHEN e_InvalidGType THEN              -- Bad GTYPE, return 0.
        REPORT_ERROR( c_cmdname, i_gtype, c_msgInvalidGType, SQLCODE, SQLERRM);
        RETURN 0;
      WHEN e_InvalidDimension THEN          -- Bad Dimension, return 0.
        REPORT_ERROR( c_cmdname, i_gtype, c_msgInvalidDimension, SQLCODE, SQLERRM);
        RETURN 0;
      WHEN OTHERS THEN                      -- General Error, return 0.
        REPORT_ERROR( c_cmdname, i_gtype, v_debug, SQLCODE, SQLERRM);
        RETURN 0;
  END GetPointRotation;
  --------------------------------------------------------------------------------------
  -- SetPointRotation sets the point rotation (in radians or degrees) of an Oracle oriented point.
  -- Syntax: UPDATE tablename A SET A.geometry=GOOM.SetPointRotation(g_geom,n_degrees);  
  --         g_geom : the actual geometry column (no quotes).
  --         n_degrees : Input rotation in degrees (0 is default).
  --  
  FUNCTION SetPointRotation( g_geom IN SDO_GEOMETRY, n_degrees IN NUMBER DEFAULT 0) 
    RETURN SDO_GEOMETRY DETERMINISTIC AS
    --
    c_cmdname             VARCHAR2(16) := 'SetPointRotation';
    i_gtype               INTEGER;
    i_dim                 INTEGER;
    i_srid                INTEGER;
    g_newgeom             SDO_GEOMETRY;
    v_debug               VARCHAR2(32) := 'Init';
  BEGIN
    -- Extract components of the input pt geometry
    i_gtype := g_geom.SDO_GTYPE;
    i_srid  := g_geom.SDO_SRID;
    i_dim   := SUBSTR( i_gtype, 1, 1);
    IF i_gtype NOT IN (2001, 3001) THEN       -- Check to make sure input geometry is a point...
      RAISE e_invalidGType;                   -- if not, raise an exception.
    END IF;
    IF g_geom.SDO_POINT IS NOT NULL THEN      -- Check if input geometry is a native point...
      RAISE e_invalidPTType;                  -- if so, raise an exception.
    END IF;
    -- Write new ordinate array based on dimensions and new point rotation.
    v_debug := 'Writing Ordinates...';
    IF i_dim = 2 THEN     -- Input geometry is 2D
      g_newgeom := SDO_GEOMETRY( i_gtype, i_srid, NULL, MDSYS.SDO_ELEM_INFO_ARRAY(1, 1, 1, 3, 1, 0), MDSYS.SDO_ORDINATE_ARRAY());
      g_newgeom.SDO_ORDINATES.EXTEND(4);
      g_newgeom.SDO_ORDINATES(1) := g_geom.SDO_ORDINATES(1);
      g_newgeom.SDO_ORDINATES(2) := g_geom.SDO_ORDINATES(2);
      g_newgeom.SDO_ORDINATES(3) := ROTINDEX('I', n_degrees);
      g_newgeom.SDO_ORDINATES(4) := ROTINDEX('J', n_degrees);
    ELSIF i_dim = 3 THEN  -- Input geometry is 3D
      g_newgeom := SDO_GEOMETRY( i_gtype, i_srid, NULL, MDSYS.SDO_ELEM_INFO_ARRAY(1, 1, 1, 4, 1, 0), MDSYS.SDO_ORDINATE_ARRAY());
      g_newgeom.SDO_ORDINATES.EXTEND(6);
      g_newgeom.SDO_ORDINATES(1) := g_geom.SDO_ORDINATES(1);
      g_newgeom.SDO_ORDINATES(2) := g_geom.SDO_ORDINATES(2);
      g_newgeom.SDO_ORDINATES(3) := g_geom.SDO_ORDINATES(3);
      g_newgeom.SDO_ORDINATES(4) := ROTINDEX('I', n_degrees);
      g_newgeom.SDO_ORDINATES(5) := ROTINDEX('J', n_degrees);
      g_newgeom.SDO_ORDINATES(6) := 0;
    ELSE
      RAISE e_invalidDimension;
    END IF;
    RETURN g_newgeom;
  EXCEPTION
    WHEN e_invalidGType THEN
      -- Bad GTYPE, return original.
      REPORT_ERROR( c_cmdname, i_gtype, c_msginvalidgtype, SQLCODE, SQLERRM);
      RETURN g_geom;
    WHEN e_invalidPTType THEN
      -- Bad point type, return original.
      REPORT_ERROR( c_cmdname, i_gtype, c_msginvalidpttype, SQLCODE, SQLERRM);
      RETURN g_geom;
    WHEN e_invalidDimension THEN
      -- Bad Dimension, return original.
      REPORT_ERROR( c_cmdname, i_gtype, c_msginvaliddimension, SQLCODE, SQLERRM);
      RETURN g_geom;
    WHEN OTHERS THEN
      -- General Error, return original.
      REPORT_ERROR(c_cmdname, i_gtype, v_debug, SQLCODE, SQLERRM);
      RETURN g_geom;
  END SetPointRotation;
  --------------------------------------------------------------------------------------
  -- ChangeOrdinatePrecision returns a geometry with the values in the ordinate array rounded 
  -- to the number of specified decimal places.  Permanently alters your data!
  -- Syntax: UPDATE table A SET A.geometry = GOOM.ChangeOrdinatePrecision(A.geometry, i_decimals);
  --         g_geom     : the actual geometry column (no quotes).
  --         i_decimals : the number of decimal places of precision to preserve, the default is 6.
  --     
  FUNCTION ChangeOrdinatePrecision( g_geometry IN SDO_GEOMETRY, i_decimals IN INTEGER DEFAULT 6) RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC IS                                                              
     i_arrayidx         INTEGER;                                                     -- Array index
     g_ordinates        MDSYS.SDO_ORDINATE_ARRAY := NEW MDSYS.SDO_ORDINATE_ARRAY(1); -- Initialize the ordinate array
     g_point            MDSYS.SDO_POINT_TYPE     := NULL;                            -- Initialize g_point
  BEGIN
    -- Handle native points differently because they are not an array
    IF ( g_geometry.SDO_POINT IS NOT NULL ) THEN
      g_point.X := Round( g_geometry.SDO_POINT.X, i_decimals );
      g_point.Y := Round( g_geometry.SDO_POINT.Y, i_decimals );
      IF TRUNC( g_geometry.SDO_GTYPE / 1000) > 2 THEN
        g_point.Z := Round( g_geometry.SDO_POINT.Z, i_decimals );
      END IF;
    END IF;
    -- Handle array based geometries, only the ordinate array is affected here.
    IF ( g_geometry.SDO_ORDINATES IS NOT NULL ) THEN                                 -- Process only if data exists in ordinate array
      g_ordinates.DELETE;                                                            -- Delete any contents in new array
      g_ordinates.EXTEND( g_geometry.SDO_ORDINATES.COUNT );                           -- Then extend to match size of existing array
      -- Process all values in the array
      FOR i_arrayidx IN 1..( g_ordinates.COUNT ) LOOP
        g_ordinates( i_arrayidx ) := Round( g_geometry.SDO_ORDINATES( i_arrayidx ), i_decimals);
      END LOOP;
    END IF;
    -- Return reconstructed geometry using rounded values in the ordinate array.
    RETURN MDSYS.SDO_GEOMETRY( g_geometry.SDO_GTYPE, g_geometry.SDO_SRID, g_point, g_geometry.SDO_ELEM_INFO, g_ordinates );
  END ChangeOrdinatePrecision;
  --------------------------------------------------------------------------------------
  -- ContainsArcs is a check for arcs in a geometry.  It returns 1 for true and 0 for false.
  -- Syntax: IF GOOM.ContainsArcs(g_geom) = 1 THEN
  --         Select GOOM.ContainsArcs(a.geometrycol) FROM table A;
  --          g_geom : the actual geometry column (no quotes).
  --
  FUNCTION ContainsArcs ( g_InputGeom IN SDO_GEOMETRY) RETURN INTEGER AS
   n_Etype        NUMBER;
   n_interpret    NUMBER;
   BEGIN
     FOR I IN g_InputGeom.SDO_ELEM_INFO.FIRST .. g_InputGeom.SDO_ELEM_INFO.LAST LOOP
       CASE
         WHEN MOD(I,3) = 1 THEN CONTINUE;
         WHEN MOD(I,3) = 2 THEN n_Etype := g_InputGeom.SDO_ELEM_INFO(I);
         WHEN MOD(I,3) = 0 THEN n_interpret := g_InputGeom.SDO_ELEM_INFO(I);
         IF(( n_Etype = 2 AND n_interpret = 2) OR ( n_Etype IN (1003,2003) AND n_interpret IN (2,4))) THEN
           RETURN 1;
         END IF;
       END CASE;
     END LOOP;
     RETURN 0;
   END ContainsArcs;

  -- -----------------------------------------------------------------------------------------------------------------
  -- TABLE AND SEQUENCE PROCEDURES: Database object creation and manipulation
  -- -----------------------------------------------------------------------------------------------------------------

  -- DropSequence drops the specified sequence if it exists.
  -- Syntax: EXEC GOOM.DropSequence(v_sequence);
  --         v_sequence : the sequence name.
  -- 
  PROCEDURE DropSequence( v_sequence IN VARCHAR2) IS
    c_cmdname   CONSTANT VARCHAR2(32) := 'DropSequence';
    c_cmdtype   CONSTANT VARCHAR2(4)  := 'MISC';
    c_feedback  CONSTANT VARCHAR2(18) := 'Sequence Deleted: ';
    -- added for owner.table support
    v_ownerseq  VARCHAR2(61);
    v_schema     VARCHAR2(30);
    v_seqname   VARCHAR2(30);
  BEGIN
    -- added for owner.table support
    v_ownerseq   := GetOwnerObject( v_sequence);
    v_schema      := SplitOwnerObject( v_ownerseq, 'OWNER');
    v_seqname    := SplitOwnerObject( v_ownerseq, 'OBJECT'); 
    --
    IF NOT chkSequence( v_seqname ) THEN
      RAISE e_SequenceNotFound;
    END IF;
    EXECUTE IMMEDIATE 'DROP SEQUENCE ' || v_ownerseq;
    Response( c_cmdname, c_feedback || v_ownerseq);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownerseq, c_feedback || v_sequence);
  EXCEPTION
    WHEN e_SequenceNotFound THEN
      Response( c_cmdname || c_msgInform, c_msgSequenceNotFound || v_ownerseq); 
      WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownerseq, c_msgSequenceNotFound || v_seqname);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownerseq, 'NONE', SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownerseq, c_msgOraError || SQLCODE);
  END DropSequence;
  --------------------------------------------------------------------------------------
  -- GetSequenceName returns a unique sequence name based on the table/column name.
  -- Syntax: v_seqname:=GOOM.GetSequenceName(v_tablename,[v_column]);
  --         v_tablename : required.
  --         v_column    : optional but is generally the column using the sequence.
  --         
  FUNCTION GetSequenceName( v_tablename IN VARCHAR2, v_column IN VARCHAR2 DEFAULT NULL, 
                            b_unique    IN BOOLEAN DEFAULT FALSE) RETURN VARCHAR2 IS
    --
    c_cmdname       CONSTANT VARCHAR2(32) := 'GetSequenceName';
    v_seqname       VARCHAR2(30);
    v_seqext        CONSTANT VARCHAR2(3) := '_SQ';
    i_count         PLS_INTEGER:=1; 
    i_tablength     PLS_INTEGER;
    i_collength     PLS_INTEGER;
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    v_table         VARCHAR2(30);
    --
  BEGIN
    -- added for owner.table support
    v_ownertable   := GetOwnerObject( v_tablename);
    v_schema        := SplitOwnerObject( v_ownertable, 'OWNER');
    v_table        := SplitOwnerObject( v_ownertable, 'TABLE');
   --
   IF b_unique THEN
       i_tablength:=NVL(LENGTH( v_table ),0);
       i_collength:=NVL(LENGTH( v_column ),0);
       IF v_column IS NOT NULL AND ( i_tablength + i_collength )<=24 THEN
          v_seqname := v_table || '_' || v_Column || v_seqext;
          WHILE ChkSequence( v_schema ||'.'|| v_seqname ) LOOP
            v_seqname := v_table ||'_'|| v_Column || i_count || v_seqext;
            i_count   := i_count + 1;
          END LOOP;
       ELSIF i_tablength > 24 THEN
            v_seqname := SUBSTR( v_table, 1, 25) || v_seqext;
            WHILE ChkSequence( v_schema ||'.'|| v_seqname) LOOP
              v_seqname := SUBSTR( v_table, 1, 25 ) || i_count || v_seqext;
              i_count   := i_count+1;
            END LOOP;
        ELSE
            v_seqname := v_table || v_seqext;
        END IF;
     ELSE
        IF v_column IS NOT NULL THEN
          v_seqname := SUBSTR( v_table, 1, 17) || '_' || SUBSTR( v_Column, 1, 10) || v_seqext;
        ELSE
          v_seqname := SUBSTR( v_table, 1, 27) || v_seqext;
        END IF;
     END IF;
    RETURN v_schema ||'.'|| v_seqname;
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_Column, v_schema||'.'|| v_seqname, SQLCODE, SQLERRM);
  END GetSequenceName;
  --------------------------------------------------------------------------------------
  -- CreateSequence creates a Sequence for specified table/column.  Sequence will start at MAX(col)+1.
  -- Syntax: EXEC GOOM.CreateSequence(v_tablename,[v_column]);
  --         v_tablename : required.
  --         v_column    : optional but is generally the column using the sequence.
  --
  PROCEDURE CreateSequence( v_tablename IN VARCHAR2, v_Column IN VARCHAR2 DEFAULT NULL) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'CreateSequence';
    c_cmdtype       CONSTANT VARCHAR2(4)  := 'MISC';
    c_feedback1     CONSTANT VARCHAR2(27) := 'Existing sequence deleted: ';
    c_feedback2     CONSTANT VARCHAR2(18) := 'Sequence created: ';
    n_MaxVal        NUMBER(38)   := 0;
    v_seqname       VARCHAR2(61);
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    --
  BEGIN
    -- added for owner.table support
    v_ownertable   := GetOwnerObject( v_tablename);
    v_schema        := SplitOwnerObject( v_ownertable, 'OWNER');
    --
    IF v_column IS NOT NULL THEN
      v_seqname := GetSequenceName( v_ownertable, v_column);
      v_sql     := 'SELECT NVL(MAX(' || v_Column || '),0) + 1 FROM ' || v_ownertable;
      EXECUTE IMMEDIATE v_sql INTO n_MaxVal;
    ELSE
      v_seqname := GetSequenceName( v_ownertable );
      n_MaxVal  := 1;
    END IF;
    IF chkSequence( v_seqname ) THEN
      DropSequence( v_seqname );
      Response( c_cmdname || c_msgWarning, c_feedback1 || v_seqname);
      WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable, c_feedback2 || v_seqname);
    END IF;
    v_sql := 'CREATE SEQUENCE ' || v_seqname || ' START WITH ' || n_MaxVal;
    EXECUTE IMMEDIATE v_sql;
    Response( c_cmdname, c_feedback2|| v_seqname);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable, c_feedback2 || v_seqname);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_Column || '<-Numeric?', v_seqname, v_sql, SQLERRM);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, v_ownertable, c_msgOraError || SQLCODE);
  END CreateSequence;
  --------------------------------------------------------------------------------------
  -- CreateNewSequence will create a new sequence for the specified table and optional
  -- columnname and will return the sequence name.
  -- Syntax: EXEC GOOM.CreateNewSequence(v_tablename,[v_column],v_seqname);
  --         v_tablename : required.
  --         v_column    : optional but is generally the column using the sequence.
  --         v_seqname   : must be declared in the calling procedure.
  --
  PROCEDURE CreateNewSequence( v_tablename IN VARCHAR2, v_Column IN VARCHAR2 DEFAULT NULL, v_seqname OUT VARCHAR2) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'CreateNewSequence';
    c_feedback2     CONSTANT VARCHAR2(18) := 'Sequence created: ';
    --
    n_MaxVal        NUMBER(38)   := 0;
    v_sql           VARCHAR2(255);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
  BEGIN
    -- added for owner.table support
    v_ownertable := GetOwnerObject( v_tablename);
    --
    IF v_column IS NOT NULL THEN
      v_seqname := GetSequenceName( v_ownertable, v_column,TRUE);
      v_sql     := 'SELECT NVL(MAX(' || v_Column || '),0) + 1 FROM ' || v_ownertable;
      EXECUTE IMMEDIATE v_sql INTO n_MaxVal;
    ELSE
      v_seqname := GetSequenceName( v_ownertable, NULL, TRUE);
      n_MaxVal  := 1;
    END IF;
    v_sql := 'CREATE SEQUENCE ' || v_seqname || ' START WITH ' || n_MaxVal;
    EXECUTE IMMEDIATE v_sql;
    Response( c_cmdname, c_feedback2 || v_seqname);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable || '.' || v_Column || '<-Numeric?', v_seqname, v_sql, SQLERRM);
  END CreateNewSequence;
  --------------------------------------------------------------------------------------
  -- DropTable drops the table and any associated sequences, indexes, Oracle metadata and GDOSYS metadata.
  -- Syntax: EXEC GOOM.DropTable(v_tablename);
  --
  PROCEDURE DropTable( v_tablename IN VARCHAR2) IS
    c_cmdname       CONSTANT VARCHAR2(32) := 'DropTable';
    c_cmdtype       CONSTANT VARCHAR2(4)  := 'MISC';
    c_feedback      CONSTANT VARCHAR2(40) := 'Deleted table and associated metadata: ';
    --
    v_seqname       VARCHAR2(61);
    -- added for owner.table support
    v_ownertable    VARCHAR2(61);
    v_schema         VARCHAR2(30);
    --
  BEGIN
    -- added for owner.table support
    v_ownertable   := GetOwnerObject( v_tablename );
    v_schema       := SplitOwnerObject( v_ownertable, 'OWNER');
    -- Check Inputs
    IF NOT chkTable( v_ownertable ) THEN
      RAISE e_TableNotFound;
    END IF;
    IF chkVIEW( v_ownertable ) THEN
      RAISE e_TableIsView;
    END IF;
    --
    EXECUTE IMMEDIATE 'DROP TABLE ' || v_ownertable||' PURGE';
    Response( c_cmdname, v_ownertable);
    v_seqname := GetSequenceName( v_ownertable );
    DropSequence( v_seqname );
    DeleteOrphanMBR( v_schema );
    DeleteOrphanCS;
    Response( c_cmdname, c_feedback || v_ownertable);
    WRITE_RESULTS( v_schema, c_cmdtype, c_cmdname, v_ownertable, c_feedback || v_ownertable);
  EXCEPTION
    WHEN e_tableNotFound THEN
      Response( c_cmdname || c_msgWarning, c_msgTableNotFound || v_ownertable);
      WRITE_RESULTS( v_schema, c_cmdname || c_msgWarning, c_cmdname, v_ownertable, c_msgTableNotFound || v_ownertable);
    WHEN e_TableIsView THEN
      Response( c_cmdname || c_msgerror, c_msgTableIsView || v_ownertable);
      WRITE_RESULTS( v_schema, c_msgError, c_cmdname, c_msgTableIsView || v_ownertable, c_msgGoomInternal);
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_ownertable, c_feedback, SQLCODE, SQLERRM);
      WRITE_RESULTS( v_schema, c_cmdname || c_msgWarning, c_cmdname, v_ownertable, c_msgOraError || SQLCODE);
  END DropTable;
  --------------------------------------------------------------------------------------
  -- CopyTable create a copy of the source table.  Options included spatial index creation, 
  -- data, and GDOSYS metadata creation.  The default is True, True, True. 
  -- EXEC GOOM.CopyTable(v_scrtable, v_tgttable, b_sidx, b_data, b_gdosys); 
  --      v_scrtable : the source table.
  --      v_tgttable : the target table.
  --      b_sidx     : when TRUE, create the spatial index, default TRUE.
  --      b_data     : when TRUE, copy the source data to the targe table, default TRUE.
  --      b_gdosys   : when TRUE, create the GDOSYS metadata entries for the target table.
  --
  PROCEDURE CopyTable( v_srcownertable IN VARCHAR2, v_tgtownertable IN VARCHAR2,
                      b_sidx          IN BOOLEAN DEFAULT TRUE,
                      b_data          IN BOOLEAN DEFAULT TRUE,
                      b_gdosys        IN BOOLEAN DEFAULT TRUE) IS
  --
  c_cmdname     CONSTANT VARCHAR2(30)    := 'CopyTable';
  --
  b_proc        BOOLEAN         := TRUE;
  v_pkey        VARCHAR2(30)    := 'NO_KEY';
  v_pkeyname    VARCHAR2(61);
  v_geomcol     VARCHAR2(30)    := NULL;
  v_sql         VARCHAR2(255);
  BEGIN
    -- Does table exist?
    IF NOT CHKTABLE( v_srcownertable ) THEN
      RESPONSE( c_cmdname || c_msgError, c_msgTableNotFound||v_srcownertable);
      b_proc := FALSE; -- if false do not process
    END IF;
    -- Does target table exist?
    IF CHKTABLE( v_tgtownertable ) THEN
      RESPONSE( c_cmdname || c_msgError, c_msgTableExists||v_tgtownertable);
      b_proc := FALSE; -- if True do not process
    END IF;
    -- If the above conditions pass and we want to copy data too, process this one
    IF b_data AND b_proc THEN
      v_pkey    := GETKEYCOL( v_srcownertable );
      v_geomcol := GETGEOM( v_srcownertable );
      v_sql     := 'CREATE TABLE ' || v_tgtownertable || ' AS SELECT * FROM ' || v_srcownertable;
      EXECUTE IMMEDIATE v_sql;
      -- If no data is to be included, process this one.
    ELSIF NOT b_data AND b_proc THEN
      v_pkey    := GETKEYCOL( v_srcownertable );
      v_geomcol := GETGEOM( v_srcownertable );
      v_sql     := 'CREATE TABLE ' || v_tgtownertable || ' AS SELECT * FROM ' || v_srcownertable || ' Where 1=0';
      EXECUTE IMMEDIATE v_sql;
    END IF;
    -- set key column for new table based on old table.  Single key only.
    IF b_proc And v_pkey != 'NO_KEY' THEN
      v_pkeyname := GETINDXNAME( v_tgtownertable, '_PK');
      v_pkeyname := SPLITOWNEROBJECT( v_pkeyname, 'OBJECT');
      v_sql      := 'ALTER TABLE ' || v_tgtownertable || ' ADD CONSTRAINT ' || v_pkeyname || ' PRIMARY KEY (' || v_pkey || ')';
      EXECUTE IMMEDIATE v_sql;
      RESPONSE( c_cmdname, 'Added primary Key: '|| v_pkeyname);
    ELSE
      RESPONSE( c_cmdname, c_msgNoKeyColumn);
    END IF;
    -- Handle the Oracle metadata and spatial indexing if indicated by parameter.
    IF b_proc And b_sidx THEN
      IF v_geomcol IS NULL THEN
        RESPONSE( c_cmdname || c_msgInform, 'No geometries to process!');
      ELSE
        SETMBRPROJ( v_tgtownertable );
        SPATIALINDEX( v_tgtownertable );
      END IF;
    END IF;
    IF b_proc And b_gdosys THEN
      SETGDOSYSMETADATA( v_tgtownertable );
    END IF;
    --
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_sql, v_srcownertable ||':'|| v_tgtownertable, c_msgOraError||SQLCODE, SQLERRM);
  END CopyTable;
  ---------------------------------------------------------------------------------------
  -- DelDupRows deletes duplicate records in a table based on the specified column.
  -- EXEC GOOM.DelDupRows(v_tablename, v_column); 
  -- This procedure is destructive, make sure you understand what it is doing.
  --
  PROCEDURE DelDupRows( v_tablename IN VARCHAR2, v_column IN VARCHAR2) IS
    c_cmdname       CONSTANT VARCHAR2(10) := 'DelDupRows';
    --
    v_sql           VARCHAR2(1024);
    i_precount      INTEGER;
    i_delcount      INTEGER;
    i_postcount     INTEGER;
    v_ownertable    VARCHAR2(61);
  BEGIN
    v_ownertable := GetOwnerObject( v_tablename ); 
    EXECUTE IMMEDIATE 'SELECT COUNT(1) FROM '|| v_ownertable INTO i_precount;
    v_sql := 'SELECT COUNT(1) FROM '|| v_ownertable ||' WHERE ROWID NOT IN 
             (SELECT MIN(ROWID) FROM '|| v_ownertable ||' GROUP BY '|| v_column ||')';
    EXECUTE IMMEDIATE v_sql INTO i_delcount;
    v_sql := 'DELETE FROM '|| v_ownertable ||' WHERE ROWID NOT IN 
             (SELECT MIN(ROWID) FROM '|| v_ownertable ||' GROUP BY '|| v_column ||')';
    EXECUTE IMMEDIATE v_sql;
    COMMIT;
    EXECUTE IMMEDIATE 'SELECT COUNT(1) FROM '|| v_ownertable INTO i_postcount;
    RESPONSE( c_cmdname,'Process results for   : '|| v_ownertable ||'.'|| v_column );
    RESPONSE( c_cmdname,'Original record count : '|| i_precount );
    RESPONSE( c_cmdname,'Duplicate record count: '|| i_delcount );
    RESPONSE( c_cmdname,'Final record count    : '|| i_postcount );
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR( c_cmdname, v_sql, v_ownertable ||'.'|| v_column, c_msgOraError|| SQLCODE, SQLERRM);
  END DelDupRows;
  ---------------------------------------------------------------------------------------
  -- AddPrimaryKey adds an integer based sequence populated primary key to an existing table.
  -- You can optional have the existing primary key preserved as a column or deleted.  If 
  -- GDOSYS metadata exists, it will be updated automatically.  This may delete any custom
  -- metadata settings.
  -- EXEC GOOM.AddPrimaryKey(v_tablename, v_keyname, b_delcurrentkey);
  -- 
  PROCEDURE AddPrimaryKey( v_table IN VARCHAR2, v_keycolnew IN VARCHAR2 DEFAULT 'PID', 
                           b_dropflag in BOOLEAN DEFAULT FALSE) IS
  c_cmdname           VARCHAR2(30):='AddPrimaryKey';
  --
  v_ownertable        VARCHAR2(61);
  v_tablename         VARCHAR2(30);
  v_schema            VARCHAR2(30);
  v_keycolold         VARCHAR2(30);
  v_keyflag           VARCHAR2(5);
  v_geomcol           VARCHAR2(30);
  v_seqname           VARCHAR2(93);
  v_sql               VARCHAR2(4096);
  i_seqexist          PLS_INTEGER;
  v_constraint        VARCHAR2(30);
  v_debug             VARCHAR2(255) := 'Start';
  v_csguid            VARCHAR2(64);
  i_ichk              PLS_INTEGER;
--
  BEGIN
    ProcessStart( c_cmdname, USER);
    --
    v_debug      := 'Variable Assignment';
    v_ownertable := GetOwnerObject( v_table );
    v_schema     := SplitOwnerObject( v_ownertable, 'OWNER');
    v_tablename  := SplitOwnerObject( v_ownertable, 'TABLE');
    v_constraint := SUBSTR( v_tablename,1,27) || c_PKext;
    v_keycolold  := GetKeyCol( v_tablename );
    v_geomcol    := GetGeom( v_tablename );
    --
    v_debug :='CHECKING GEOMETRY';
    IF v_geomcol is NULL THEN
      RESPONSE( v_debug, 'No geometry column found.');
    ELSE
      RESPONSE( v_debug, 'Geometry Column: '|| v_geomcol ||' found.');
      IF ChkMetadata( v_tablename ) THEN
        RESPONSE('CHECKING GEOMETRY','Storing Coordinate System.');
        v_sql:='SELECT CSGUID FROM GDOSYS.GFIELDMAPPING WHERE OWNER=USER AND TABLE_NAME=:vtabname AND COLUMN_NAME=:vgeomcol';
        EXECUTE IMMEDIATE v_sql INTO v_csguid USING v_tablename, v_geomcol;
        DelGDOSYSMetadata( v_tablename );
      ELSE
        v_csguid:='D';
      END IF;
    END IF;
    IF v_keycolold = 'NO_KEY' THEN
      v_keyflag:='FALSE';
    ELSE
      v_keyflag:='TRUE';
    END IF;
    v_sql:='SELECT COUNT(1) FROM COLS WHERE TABLE_NAME=:vtablename AND COLUMN_NAME=:vkeycolnew';
    EXECUTE IMMEDIATE v_sql INTO i_ichk USING v_tablename, v_keycolnew;
    IF i_ichk <> 1 THEN
      v_sql:='ALTER TABLE '|| v_tablename ||' ADD ('|| v_keycolnew ||' INTEGER)';
      EXECUTE IMMEDIATE v_sql;
      v_debug:='Key column: '|| v_keycolnew ||' added to table '|| v_tablename;
      RESPONSE('ALTER TABLE', v_debug);
    END IF;
    IF v_keyflag = 'TRUE' THEN
      v_sql:='ALTER TABLE '|| v_tablename ||' DROP PRIMARY KEY';
      EXECUTE IMMEDIATE v_sql;
      v_debug:='Current Primary Key Deleted.';
      RESPONSE('ALTER TABLE', v_debug);
    END IF;
    CreateNewSequence( v_tablename, v_keycolnew, v_seqname );
    v_sql:='UPDATE '|| v_tablename ||' SET '|| v_keycolnew ||'='|| v_seqname ||'.NEXTVAL';
    EXECUTE IMMEDIATE v_sql;
    COMMIT;
    v_debug:='Column '|| v_keycolnew ||' updated from sequence '|| v_seqname ||'...';
    RESPONSE('UPDATE TABLE',v_debug);
    v_sql:='ALTER TABLE '|| v_tablename ||' MODIFY '|| v_keycolnew ||' NOT NULL';
    EXECUTE IMMEDIATE v_sql;
    v_debug:='Column '|| v_keycolnew ||' modified to NOT NULL...';
    RESPONSE('ALTER TABLE', v_debug);
    v_sql:='ALTER TABLE '|| v_tablename ||' ADD CONSTRAINT '|| v_constraint ||' PRIMARY KEY ('|| v_keycolnew ||')';
    EXECUTE IMMEDIATE v_sql;
    v_debug:='Constraint '|| v_constraint ||' is your new primary key constraint.';
    RESPONSE('ALTER TABLE', v_debug);
    IF b_dropflag AND v_keyflag = 'TRUE' THEN
      EXECUTE IMMEDIATE 'ALTER TABLE '|| v_tablename ||' DROP ('|| v_keycolold||')';
      v_debug:='Old Key Column '|| v_keycolold ||' Dropped.';
      RESPONSE('ALTER TABLE', v_debug);
    END IF;
    SetGDOSYSMetadata( v_tablename, v_seqname, v_csguid);
    ProcessComplete( c_cmdname, USER);
  EXCEPTION
    WHEN OTHERS THEN
      REPORT_ERROR ( c_cmdname, v_sql, v_debug, sqlcode, sqlerrm);
  END AddPrimaryKey;
  
  -- -----------------------------------------------------------------------------------------------------------------
  -- ONLINE HELP
  -- -----------------------------------------------------------------------------------------------------------------

  -- HelpSyntax returns the syntax for the online help.
  -- Syntax: EXEC GOOM.HelpSyntax;
  --
  PROCEDURE HelpSyntax IS
  BEGIN
    TitleLine('GOOM HELP SYNTAX');
    DotLine;
    DBMSG('v_  indicates varchar2 literal in upper case - ''TABLENAME'' or ''OWNER''       ');
    DBMSG('n_  indicates numeric - 1.3 or 12432.0                                          ');
    DBMSG('i_  indicates integer - 1 or 3 or 12432.                                        ');
    DBMSG('b_  indicates boolean - TRUE or FALSE                                           ');
    DBMSG('[ ] indicate an optional value.                                                 ');
    DBMSG('<geometry> or g_ indicates geometry object, this is not a literal.              ');
    DBMSG('Common usage:                                                                   ');
    DBMSG('v_ownertable - Name of the table to use: ''TABLENAME'' or ''OWNER.TABLENAME'''   );
    DBMSG('v_geomcol    - Name of the SDO_GEOMETRY column: Default is NULL in most cases.  ');
    DBMSG('-              If the table contains only one geometry column, NULL will force  ');
    DBMSG('-              the geometry column to be determined automatically.              ');
    DBMSG('v_schema     - Name of schema to process: ''CHUCK''                             ');
    DBMSG('g_geometry   - You can pass the actual geometry column, a variable containing a ');
    DBMSG('-              sdo_geometry construct or the construct itself.                  ');
    DBMSG('Notes:                                                                          ');
    DBMSG('-> OWNER.TABLE input is allowed for most procedure and functions except where   ');
    DBMSG('-  indicated.  If the owner is not specified, the default is always assumed to  ');
    DBMSG('-  be the current USER or schema. Always remember to use single quotes around   ');
    DBMSG('-  literals. You can also pass the USER built in variable.                      ');
    DBMSG('-> In most cases, it is assumed the DBA is the user running in OWNER.TABLE mode.');
    DBMSG('-> Standard users may not have all the privileges required to run all procedures');
    DBMSG('-  on tables they do not own directly but have limited access to.               ');
    DBMSG('-> When specific tables are not used, the command generally applies to all      ');
    DBMSG('-  feature classes in the schema.                                               ');
  END HelpSyntax;
  ----------------
  -- HelpHDR is the online help header information.
  -- Syntax:  EXEC HelpHDR;
  --
  PROCEDURE HelpHDR IS
  BEGIN
    DblLine;
    TitleLine('GOOM Package Online Help');
    DotLine;
    DBMSG('For help syntax enter EXEC GOOM.HelpSyntax;                     ');
    DBMSG('For a list of common procedures enter EXEC GOOM.HELPME;      ');
    DBMSG('For a details on a specific topic, enter one of the following:  ');
    DBMSG('EXEC GOOM.HelpDATA;           -- Spatial Data Manipulation');
    DBMSG('EXEC GOOM.HelpMBR;            -- Oracle metadata procedures.           ');
    DBMSG('EXEC GOOM.HelpGDOSYS;         -- Geomedia metadata (GDOSYS) procedures.');
    DBMSG('EXEC GOOM.HelpTUNING;         -- Spatial Indexing and tuning.');
    DBMSG('EXEC GOOM.HelpUTILITIES;      -- Miscellaneous useful stuff.');
    DBMSG('EXEC GOOM.HelpVALIDATION;     -- Validation and repair procdures.');
    DBMSG('EXEC GOOM.HelpFUNCTIONS;      -- GOOM utility functions.');
  END;
  ----------------
  -- HelpData is detailed help on data procedures and functions.
  -- Syntax:  EXEC HelpDATA;
  --
  PROCEDURE HelpDATA IS
  BEGIN
    HelpHdr;
    DashLine;
    HelpSyntax;
    TitleBlock('Data - Spatial Data Manipulation and Modification');
    DBMSG('FUNCTION Bearing(n_xcoord1 IN NUMBER,');
    DBMSG('.                n_ycoord1 IN NUMBER,');
    DBMSG('.                n_xcoord2 IN NUMBER,');
    DBMSG('.                n_ycoord2 IN NUMBER,');
    DBMSG('.                v_ctype   IN VARCHAR2 DEFAULT ''P'',');
    DBMSG('.                v_rtype   IN VARCHAR2 DEFAULT ''D'')' );
    DBMSG('.       RETURN NUMBER');
    DBMSG('.');
    DBMSG('Bearing returns the bearing between start (1) and end (2) points.');
    DBMSG('Syntax: n_bearing := BEARING(x1,y1,x2,y2,v_ctype,v_rtype);');
    DBMSG('.       v_ctype - Default ''P'' for projected.  Otherwise calculates for long lat.');
    DBMSG('.       v_rtype - Default ''D'' for degrees.  Otherwise calculates radians.'); 
    DotLine;
    DBMSG('FUNCTION ChangeOrdinatePrecision(g_geom IN SDO_GEOMETRY, '); 
    DBMSG('.                                i_decimals IN INTEGER DEFAULT 6) '); 
    DBMSG('.        RETURN MDSYS.SDO_GEOMETRY;'); 
    DBMSG('.');
    DBMSG('ChangeOrdinatePrecision returns a geometry with the values in the ordinate array ');
    DBMSG('rounded to the number of specified decimal places.  Permanently alters your data! ');
    DBMSG('Syntax: UPDATE table A SET A.geometry = GOOM.ChangeOrdinatePrecision(A.geometry, i_decimals);');
    DBMSG('.       g_geom is the actual geometry column (no quotes).');
    DBMSG('.       i_decimals is the number of decimal places of precision to preserve, the default is 6.');
    DotLine;
    DBMSG('FUNCTION ContainsArcs(g_InputGeom IN SDO_GEOMETRY) RETURN INTEGER;');
    DBMSG('.');
    DBMSG('ContainsArcs is a check for arcs in a geometry.  It returns 1 for true and 0 for false.');
    DBMSG('Syntax: IF GOOM.ContainsArcs(g_geom) = 1 THEN');
    DBMSG('        Select GOOM.ContainsArcs(a.geometrycol) FROM table A;');
    DBMSG('.  g_geom is the actual geometry column (no quotes).;');
    DotLine; 
    DBMSG('PROCEDURE ConvPoly2Line(v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL);'); 
    DBMSG('.');
    DBMSG('ConvPoly2Line takes an input polygon feature class and converts it to a polyline geometry');
    DBMSG('feature class.  The entire feature class is converted.  If you want to convert just a single');
    DBMSG('geometry, use Oracle''s SDO_UTIL.POLYGONTOLINE. ');
    DBMSG('Syntax: EXEC GOOM.ConvPoly2Line(v_tablename,v_geomcol);');
    DBMSG('.  v_tablename is the table being modified.');
    DBMSG('.  v_geomcol is the column containing the geometry.');
    DBMSG('Note: This command does not support owner.table.');
    DotLine;         
    DBMSG('FUNCTION Convert2D(g_3DGeom IN MDSYS.SDO_GEOMETRY) RETURN MDSYS.SDO_GEOMETRY;');
    DBMSG('.');
    DBMSG('Convert2D returns a 2D geometry based on a 3D input geometry.  The Z is stripped off.');
    DBMSG('If an error occurs, the original input geometry is returned.');
    DBMSG('Syntax: SELECT GOOM.Convert2D(GEOMETRY) FROM TABLE;');
    DBMSG('.       UPDATE TABLE SET GEOMETRY=GOOM.Convert2D(GEOMETRY);');
    DBMSG('.       g_2DGeom:=GOOM.Convert2D(3dgeometry);');
    DBMSG('Note: This command does not support owner.table.  Raster and Text are not supported.');
    DotLine;
    DBMSG('FUNCTION Convert3D(g_2DGeom IN MDSYS.SDO_GEOMETRY, n_Zval IN NUMBER DEFAULT 0) ');
    DBMSG('.        RETURN MDSYS.SDO_GEOMETRY;');
    DBMSG('.');
    DBMSG('Convert3D returns a 3D geometry based on a 2D input geometry.  Z is populated by a');
    DBMSG('constant Z value (default = 0). If an error occurs, the original input geometry is');
    DBMSG('returned.');
    DBMSG('Syntax: SELECT Convert3D(GEOMETRY, 0) FROM TABLE;');
    DBMSG('.       UPDATE TABLE SET GEOMETRY=Convert3D(GEOMETRY,0);');
    DBMSG('.       g_3DGeom:=Convert3D(g_2dGeom,0);');
    DBMSG('Note: This command does not support owner.table.  Raster and Text are not supported.');
    DotLine;
    DBMSG('FUNCTION Convert2NativePt(g_geom IN MDSYS.SDO_GEOMETRY, i_dim IN INTEGER DEFAULT 2) ');
    DBMSG('.        RETURN MDSYS.SDO_GEOMETRY;');
    DBMSG('.');    
    DBMSG('Convert2NativePt returns a 2D/3D native point geometry from an input 2D/3D oriented ');
    DBMSG('point geometry.');
    DBMSG('Syntax: g_geom:= GOOM.Convert2NativePt(g_geom,[i_dim]);');                 
    DBMSG('.       UPDATE table A SET A.geometry = GOOM.Convert2NativePt(A.geometry);');
    DBMSG('.  g_geom is the actual geometry (no quotes).    ');             
    DBMSG('.  i_dim is the optional output dimension (2 or 3). Default=2.       ');
    DotLine;
    DBMSG('FUNCTION Convert2OrientedPt(g_geom IN MDSYS.SDO_GEOMETRY) RETURN MDSYS.SDO_GEOMETRY;');
    DBMSG('.');  
    DBMSG('Convert2OrientedPt returns a 2D/3D oriented point geometry from an input 2D/3D native');
    DBMSG('pt geometry. The orientation matrix is set to 0.');
    DBMSG('Syntax: g_geom:= GOOM.Convert2OrientedPt(g_geom);');                 
    DBMSG('.       UPDATE table A SET A.geometry = GOOM.Convert2NativePt(A.geometry);');
    DBMSG('.       UPDATE table A SET A.geometry = GOOM.Convert2OrientedPt(A.geometry);');             
    DBMSG('.  g_geom is the actual point geometry (no quotes).       '); 
    DotLine; 
    DBMSG('FUNCTION ConvGeom2Pts(g_polygeom IN MDSYS.SDO_GEOMETRY) RETURN MDSYS.SDO_GEOMETRY;');
    DBMSG('.');  
    DBMSG('Returns a point cluster geometry from an input line or area geometry.');
    DBMSG('Syntax: g_geom:= GOOM.ConvGeom2Pts(g_polygeom);');                 
    DBMSG('.       UPDATE table A SET A.geometry = GOOM.ConvGeom2Pts(A.geometry);');
    DBMSG('.  g_geom is the actual point geometry (no quotes).       ');
    DotLine;
    DBMSG('FUNCTION GetPtCoord(g_geom IN MDSYS.SDO_GEOMETRY, v_typ IN VARCHAR2) RETURN NUMBER;');
    DBMSG('.');  
    DBMSG('GetPtCoord returns the ordinate associated with the specified type: X,Y, or Z.');
    DBMSG('Syntax: n_coord:=GOOM.GetPtCoord(g_geom,v_type);');
    DBMSG('.       SELECT GOOM.GetPtGeom(A.geometry,v_type) FROM table A;');
    DBMSG('.  v_typ is the coordinate to return: ''X'',''Y'', or ''Z''');
    DotLine;
    DBMSG('FUNCTION GetLinearCoord(g_geom IN MDSYS.SDO_GEOMETRY, ');
    DBMSG('.                       v_typ IN VARCHAR2,');
    DBMSG('.                       v_loc IN VARCHAR2 DEFAULT ''START'') RETURN NUMBER');
    DBMSG('.');      
    DBMSG('GetLinearCoord returns the ordinate associated with the specified type: X,Y, ');
    DBMSG('or Z from either the start or the end point of the line.');
    DBMSG('Syntax: n_coord:=GOOM.GetLinearCoord(g_geometry,v_typ,v_loc);');
    DBMSG('.       SELECT GOOM.GetLinearCoord(A.geometry,''X'',''START'') FROM table A;');
    DBMSG('.  g_geometry is the actual geometry column (no quotes).');
    DBMSG('.  v_typ  - Coordinate to return: ''X'',''Y'', or ''Z''');
    DBMSG('.  v_loc  - Location of Coord: ''START'',''END'', START is DEFAULT');
    DBMSG('Example: SELECT GetLinearCoord(A.GEOMETRY,''X'',''START'') FROM LINE_TABLE A;');
    DotLine;
    DBMSG('FUNCTION GetPtBearing(g_geom1 IN SDO_GEOMETRY, g_geom2 IN SDO_GEOMETRY,');
    DBMSG('.                     v_rtype IN VARCHAR2 DEFAULT ''D'') RETURN NUMBER;');
    DBMSG('.');     
    DBMSG('GetPtBearing returns the bearing (in radians or degrees) between input ');
    DBMSG('point 1 and input point 2.');
    DBMSG('Syntax: n_bearing:= GOOM.GetPtBearing(g_ptgeom1,g_ptgeom1, v_rtype)');
    DBMSG('.  g_ptgeom1 and g_ptgeom2 are actual geometries, no quotes');
    DBMSG('.  v_rtype determine result in degrees or radians.  D (degrees) is the default.');
    DotLine;
    DBMSG('FUNCTION GetLinearBearing(g_geom IN SDO_GEOMETRY,');
    DBMSG('.                         v_rtype IN VARCHAR DEFAULT ''D'') RETURN NUMBER;');
    DBMSG('.');
    DBMSG('GetLinearBearing returns the bearing (in radians or degrees) between the ');
    DBMSG('start point and end point of the input linear geometry.');
    DBMSG('Syntax: n_bearing:= GOOM.GetLinearBearing(g_geom,v_rtype);');
    DBMSG('.  Select GOOM.GetLinearBearing(<GEOMETRY>, v_rtype) FROM DUAL;');
    DBMSG('.  g_geom is the actual geometry column (no quotes).');
    DBMSG('.  v_rtype determines result in degrees or radians.  D (degrees) is the default.');
    DotLine;
    DBMSG('FUNCTION GetPointRotation(g_geom IN SDO_GEOMETRY,');
    DBMSG('.                         v_type IN VARCHAR2 DEFAULT ''D'') RETURN NUMBER;');
    DBMSG('GetPointRotation returns the rotation (in radians or degrees) of an oriented point.');
    DBMSG('Syntax: n_rotation:= GOOM.GetPointRotation(g_geom,v_type);');
    DBMSG('.       Select GOOM.GetPointRotation(A.<GEOMETRY>, v_type) FROM table A;');
    DBMSG('.       g_geom is the actual geometry column (no quotes).');
    DBMSG('.       v_type determines result in degrees or radians.  D (degrees) is the default.');
    DBMSG('.');
    DBMSG('FUNCTION SetPointRotation(g_geom IN SDO_GEOMETRY,');
    DBMSG('.                         n_degrees IN NUMBER DEFAULT 0) RETURN SDO_GEOMETRY;');
    DBMSG('SetPointRotation sets the point rotation of an Oracle oriented point.');
    DBMSG('Syntax: UPDATE tablename A SET A.geometry=GOOM.SetPointRotation(g_geom,n_degrees);');
    DBMSG('.       g_geom : the actual geometry column (no quotes).');
    DBMSG('.       n_degrees : Input rotation in degrees (0 is default).');
    DBMSG('.');   
    DotLINE;
    DashLine;
  END HelpDATA;
  ----------------
  -- HelpGDOSYS is detailed help on GDOSYS related procedures.
  -- Syntax:  EXEC HelpGDOSYS;
  --
  PROCEDURE HelpGDOSYS IS
  BEGIN
    HelpHDR;
    DashLine;
    HelpSyntax;
    TitleBlock('GOOM Package METADATA Procedures - Maintain GDOSYS metadata tables.');
    DBMSG('PROCEDURE SetGDOSYSMetadata(v_tablename   IN VARCHAR2,)');
    DBMSG('.                           v_seq_name    IN VARCHAR2 DEFAULT NULL,)');
    DBMSG('.                           v_cs          IN VARCHAR2 DEFAULT ''DEFAULT'',)');
    DBMSG('.                           i_geomtype_in IN INTEGER DEFAULT NULL,)');
    DBMSG('.                           v_keycolumn   IN VARCHAR2 DEFAULT NULL,)');
    DBMSG('.                           v_num38       IN VARCHAR2 DEFAULT ''LONG'',)');
    DBMSG('.                           v_num10       IN VARCHAR2 DEFAULT ''DOUBLE''))');
    DBMSG('.');
    DBMSG('SetGDOSYSMetadata sets default entries in GDOSYS for the specified table/view.  This creates');
    DBMSG('the feature class metadata used by GeoMedia.  It is the command line equivalent of running ');
    DBMSG('Database Utilities and just accepting the defaults. Only tables with a single primary key  ');
    DBMSG('are supported.  A DBA must use owner.table for the tablename.');
    DBMSG('Syntax: EXEC GOOM.SetGDOSYSMetadata(v_tablename,[v_seqname],[v_cs],[v_geomType],[v_keycolumn],');
    DBMSG('.                                  [v_num38],[v_num10]);');
    DBMSG('.  v_tablename is the name of the table or view.  It is required.');
    DBMSG('.  v_seqname (optional) is the name of the sequence that populates the primary key.  If');
    DBMSG('.    it does not exist, it will be created.');
    DBMSG('.  v_cs (optional, default D) is the csguid of the coordinate system to assign.  Use ''D'' to use');
    DBMSG('.    the default coordinate system assigned to the schema.)');
    DBMSG('.  v_geomType (optional) is the GTYPE to use for empty geometries.  Otherwise the GTYPE will');
    DBMSG('.    be picked up automatically by sampling the geometry gtype.');
    DBMSG('.  v_keycolumn (optional) used only if v_tablename is actually a view, then it');
    DBMSG('.    must be the key preserved column used in the view.');
    DBMSG('.  v_num38 (optional) is an override for NUMBER(38) PKEY assignments.  Any value here');
    DBMSG('.    except ''LONG'' forces override.  Use only if you need interoperability with ESRI NUMBER(38).');
    DBMSG('.  v_num10 (optional) is an override for all NUMBER(10) assignments.  Forces LONG.');
    DBMSG('.    Any value here except ''DOUBLE'' forces override.  Use only if you need interoperability');
    DBMSG('.    with GTECH NUMBER(10) for G3E columns.');
    DotLINE;
    DBMSG('PROCEDURE SetGDOSYSMetadataAll(v_schema IN VARCHAR2 DEFAULT USER))');
    DBMSG('.');
    DBMSG('SetGDOSYSMetadataAlL sets default entries in GDOSYS for all tables/views in your schema.');
    DBMSG('Syntax: EXEC GOOM.SetGDOSYSMetadataAll; or EXEC GOOM.SetGDOSYSMetadataAll([v_schema]);');
    DBMSG('.       Set the USER constants prior to compiling to control how the procedure works.');
    DBMSG('.       v_schema is optional and can be used by a DBA to operate on any schema.');
    DotLINE;
    DBMSG('PROCEDURE SetDefaultCS(v_csguid IN VARCHAR2 DEFAULT NULL, v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('SetDefaultCS sets the default coordinate system used by the specified schema. Setting a NULL ');
    DBMSG('for v_csguid disables the default. The CSGUID comes from the GDOSYS.GCOORDSYSTEM table.');
    DBMSG('Syntax: EXEC GOOM.SetDefaultCS(c_csguid, [v_schema]);');
    DBMSG('.       v_schema is optional and can be used by a DBA to operate on any schema.');
    DotLINE;
    DBMSG('PROCEDURE SetPrimaryGeom(v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2)');
    DBMSG('.');
    DBMSG('SetPrimaryGeom sets the primary geometry indication in GDOSYS for the specified geometry.');
    DBMSG('Syntax: EXEC GOOM.SetPrimaryGeom(v_tablename, v_geomcol);');
    DBMSG('.       v_tablename can be owner.table or just table.');
    DBMSG('.       v_geomcol is the name of the geometry in v_tablename that will be primary.');
    DBMSG('Note: This is not required for tables that contain only one geometry.');
    DotLINE;
    DBMSG('FUNCTION GetPrimaryGeom(v_tablename IN VARCHAR2) RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('GetPrimaryGeom returns the primary geometry field name used by GeoMedia.  This is ');
    DBMSG('only necessary when more than one geometry when more than one geometry is present ');
    DBMSG('in a feature class.');
    DBMSG('Syntax: EXEC GOOM.SetPrimaryGeom(v_tablename, v_geomcol);');
    DBMSG('.       v_tablename can be owner.table or just table.');
    DBMSG('Note: This is not required for tables that contain only one geometry.');
    DotLINE;
    DBMSG('PROCEDURE OverrideNumDatatype(v_tablename IN VARCHAR2, v_column IN VARCHAR2,');
    DBMSG('.                             v_type IN VARCHAR2)');
    DBMSG('.');
    DBMSG('OverrideNumDatatype allows you to set the data type matching override in GDOSYS.GFIELDMAPPING for');
    DBMSG('the specified table/column.');
    DBMSG('Syntax: EXEC GOOM.OverrideNumDatatype(v_tablename, v_column, v_type);');
    DBMSG('.       v_type can be 1 for BOOLEAN, 3 for INTEGER, 4 for LONG, or 7 for DOUBLE');
    DBMSG('Note:  This only works for numeric data types.');
    DotLINE;
    DBMSG('PROCEDURE SetField2Hypertext(v_tablename IN VARCHAR2, v_column IN VARCHAR2)');
    DBMSG('.');
    DBMSG('SetField2Hypertext allows you to set a field format to HyperText for the specified table/column ');
    DBMSG('for use in GeoMedia.  The command modifies existing GDOSYS metadata.');
    DBMSG('Syntax: EXEC GOOM.SetField2Hypertext(v_tablename, v_column);');
    DBMSG('Note:  This only works for VARCHAR2 and CHAR data types.');
    DotLINE;
    DBMSG('PROCEDURE DelGDOSYSMetadata(v_tablename IN VARCHAR2, b_respn IN BOOLEAN DEFAULT TRUE)');
    DBMSG('.');
    DBMSG('DelGDOSYSMetadata deletes all the GDOSYS metadata associated with the specified table/view.');
    DBMSG('Syntax: EXEC GOOM.DelGDOSYSMetadata(v_table_name);');
    DBMSG('.       v_tablename can be passed as a table/view for the current user.  A DBA user can');
    DBMSG('.       pass owner.table.');
    DotLINE;
    DBMSG('PROCEDURE DelGDOSYSOrphans(v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('DelGDOSYSOrphans searches for orphan entries in GDOSYS related to the specified schema.');
    DBMSG('Syntax: EXEC GOOM.DelGDOSYSOrphans; or EXEC GOOM.DelGDOSYSOrphans(v_schema);');
    DBMSG('.       v_schema is optional, normally the current user is used.  A DBA can specify');
    DBMSG('.       any schema.');
    DBMSG('PROCEDURE DBADelGDOSYSOrphans');
    DBMSG('.');
    DBMSG('DBADelGDOSYSOrphans is for DBA use only.  It will remove any GDOSYS metadata for schemas');
    DBMSG('that no longer exist in the database.  Normally this is not necessary but is useful when');
    DBMSG('GDOSYS has been exported from one instnace and imported to another.');
    DBMSG('Syntax: EXEC GOOM.DBADelGDOSYSOrphans;');
    DotLINE;
    DBMSG('PROCEDURE DeleteOrphanCS(b_respn IN BOOLEAN DEFAULT TRUE)');
    DBMSG('.');
    DBMSG('DeleteOrphanCS deletes orphan coordinate systems from GDOSYS.GCOORDSYSTEM.  If the the ');
    DBMSG('coordinate system is used anywhere in GDOSYS, it will not be deleted.');
    DBMSG('Syntax: EXEC GOOM.DeleteOrphanCS;');
    DotLINE;
    DBMSG('PROCEDURE ClearModLog(v_user IN VARCHAR2 DEFAULT NULL)');
    DBMSG('.');
    DBMSG('CleanModLog is used to truncate the GDOSYS.ModificationLog and ModifiedTables tables.');
    DBMSG('Syntax: EXEC GOOM.ClearModLog;');
    DBMSG('.       Generally this should be run by a DBA user or as a system level job.  An option');
    DBMSG('.       was added to allow EXEC GOOM.ClearModLog(USER); but this will be slow.');
    DotLINE;
    DBMSG('PROCEDURE LogDataModification(v_tablename IN VARCHAR2, v_keyname IN VARCHAR2, ');
    DBMSG('                              v_keyvalue IN VARCHAR2, i_modtype IN INTEGER)');
    DBMSG('.');
    DBMSG('LogDataModification will log and insert, update, or delete operations into GDOSYS''s ');
    DBMSG('MODIFIEDTABLES and MODIFICATIONLOG tables. This is useful when writing modlog ');
    DBMSG('triggers or doing customization that requires external notification for GM users.');
    DBMSG('Syntax: EXEC GOOM.LogDataModification(v_tablename, v_keyname, i_keyvalue, i_modtype);');
    DBMSG('.    v_tablename is the table being modified.');
    DBMSG('.    v_keyname is the primary key column name for the table.');
    DBMSG('.    v_keyvalue is the primary key row identifier being modified (integer or character).');
    DBMSG('.    i_modtype is the type of modification:  1 for insert, 2 for update, and 3 for delete.');
    DBMSG('Note:  The procedure only works with single, numeric based primary keys.');
    DashLine;
  END HelpGDOSYS;
  ----------------
  -- HelpMBR is detailed help on Oracel metadata related procedures.
  -- Syntax:  EXEC HelpMBR;
  --
  PROCEDURE HelpMBR IS
  BEGIN
    HelpHDR;
    DashLine;
    HelpSyntax;
    TitleBlock('MBR - Spatial Metadata Operations on MDSYS.SDO_GEOM_METADATA_TABLE');
    DotLine;
    DBMSG('PROCEDURE SetMBR(v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL,');
    DBMSG('.                i_dimension IN INTEGER DEFAULT NULL);');
    DBMSG('.');
    DBMSG('SetMBR sets the default MBR for a table''s geometry column in USER_SDO_GEOM_METADATA.');
    DBMSG('Syntax: EXEC GOOM.SetMBR(v_tablename,[v_geomcol],[i_dimension]);');
    DBMSG('.  v_geomcol is the geometry column.  If not specified, the procedure will automatically');
    DBMSG('.            use the first one it finds.');
    DBMSG('.  i_dimension is the dimension to use, only specify this (2 or 3) if the geometry is empty.');
    DotLINE;
    DBMSG('PROCEDURE SetMBRAll(v_schema IN VARCHAR2 DEFAULT USER, i_dimension IN INTEGER DEFAULT NULL);');
    DBMSG('.');
    DBMSG('SetMBRAll sets the default MBR in USER_SDO_GEOM_METADATA for all geometry columns in the ');
    DBMSG('schema.');
    DBMSG('Syntax: EXEC GOOM.SetMBRAll([v_schema]);');
    DBMSG('.   v_schema is optional and should only be used when a DBA is running the command for');
    DBMSG('.     the specified user.');
    DBMSG('.   i_dimension is the optional dimension to use, only specify this (2 or 3) if the geometry');
    DBMSG('.     is empty.');
    DotLINE;
    DBMSG('PROCEDURE SetMBRGeo(v_tablename IN VARCHAR2 DEFAULT ''ALL'', ');
    DBMSG('.                   i_dimension IN INTEGER DEFAULT NULL)');
    DBMSG('.');
    DBMSG('SetMBRGeo sets USER_SDO_MEOM_METADATA to +-180, +-90 for all feature classes.');
    DBMSG('Syntax: EXEC GOOM.SetMBRGeo([v_tablename],[i_dimension]);');
    DBMSG('.       v_tablename is optional.  If set, only the table name is processed, otherwise all');
    DBMSG('.         tables in the schema are processed automatically.');
    DBMSG('.       i_dimension is the optional dimension to use, only specify this (2 or 3) if ');
    DBMSG('.         the geometry is empty.');
    DotLINE;
    DBMSG('PROCEDURE SetMBRProj(v_tablename IN VARCHAR2 DEFAULT ''ALL'',');
    DBMSG('.                    i_dimension IN INTEGER DEFAULT NULL)');
    DBMSG('.');
    DBMSG('SetMBRProj set the default MBR in USER_SDO_GEOM_METADATA to the default for projected');
    DBMSG('data +-2147483648, ');
    DBMSG('+-2147483648 for all feature classes.');
    DBMSG('Syntax: EXEC GOOM.SetMBRProj or EXEC GOOM.SetMBRProj([v_tablename],[i_dimension]);');
    DBMSG('.       i_dimension is the optional dimension to use, only specify this (2 or 3) if the');
    DBMSG('.       geometry is empty.');
    DotLINE;
    DBMSG('PROCEDURE CopyMBR(v_frmtable IN VARCHAR2, v_frmgeom IN VARCHAR2, ');
    DBMSG('.                 v_totable IN VARCHAR2, v_togeom IN VARCHAR2)');
    DBMSG('.');
    DBMSG('CopyMBR copies the USER_SDO_GEOM_METADATA entries for one table/geometry to another one.');
    DBMSG('Syntax: EXEC GOOM.CopyMBR(v_source_table,v_source_geom,v_target_table,v_target_geom);');
    DBMSG('.       Input the source table/geometry and the target table/geometry.  You can also');
    DBMSG('.       specify a view.');
    DotLINE;
    DBMSG('PROCEDURE DeleteOrphanMBR(v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('DeleteOrphanMBR deletes orphan metadata from USER_SDO_GEOM_METADATA.  Orphans would occur');
    DBMSG('if you had entries for table/views that were no longer in the schema.');
    DBMSG('Syntax: EXEC GOOM.DeleteOrphanMBR;');
    DotLINE;
    DBMSG('PROCEDURE SetSRID(v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, ');
    DBMSG('.                 i_srid IN INTEGER DEFAULT 0)');
    DBMSG('.');
    DBMSG('SetSRID sets the SRID value for the specified table/geometry to the specified SRID.');
    DBMSG('SRID''s are not required for projected data but they are required for geodetic data.');
    DBMSG('Syntax: EXEC GOOM.SetSRID(v_tablename,v_geomcol,i_srid) ');
    DBMSG('Note:   i_srid=0 sets NULL value (Default) generally used for projected data.');
    DotLINE;
    DBMSG('PROCEDURE SetSRIDAll(i_srid IN INTEGER DEFAULT 0, v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('SetSRIDAll sets the specified SRID for all tables/geometries in the schema.');
    DBMSG('Syntax: EXEC GOOM.SetSRIDAll(i_srid, [v_schema]);');
    DBMSG('.      v_schema is optional and can be used by a DBA to operate on any schema.');
    DBMSG('Note:   i_srid=0 sets NULL value (Default) generally used for projected data.');
    DotLINE;
    DBMSG('PROCEDURE SetSpatialTolerance(v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, ');
    DBMSG('                              n_tol IN NUMBER DEFAULT 0.00005)');
    DBMSG('.');
    DBMSG('SetSpatialTolerance sets the Oracle spatial tolerance to use for the specified table/');
    DBMSG('geometry (USER_SDO_GEOM_METADATA).  The default tolerance is 0.00005 m and is the ');
    DBMSG('recommended value to use for projected data.');
    DBMSG('Syntax: EXEC GOOM.SetSpatialTolerance(v_tablename,v_geometry,n_tol);');
    DotLINE;
    DBMSG('PROCEDURE SetSpatialToleranceAll(n_tol IN NUMBER DEFAULT 0.00005, ');
    DBMSG('.                                v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('SetSpatialToleranceAll sets the Oracle spatial tolerance for all table/geometries');
    DBMSG('in the schema.  The default tolerance is 0.00005 m and is the recommended value ');
    DBMSG('to use for projected data.');
    DBMSG('Syntax: EXEC GOOM.SetSpatialToleranceAll(n_tol, [v_schema]);');
    DBMSG('.      v_schema is optional and can be used by a DBA to operate on any schema.');
    DotLINE;
    DBMSG('FUNCTION GetSpatialTolerance(v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL,');
    DBMSG('                             v_dim IN VARCHAR2 DEFAULT ''X'') RETURN NUMBER');
    DBMSG('.');
    DBMSG('GetSpatialTolerance returns the current tolerance setting for the table/geometry.');
    DBMSG('Syntax: SELECT GOOM.GetSpatialTolerance(v_tablename,v_geometry,[v_dim]) FROM DUAL;');
    DBMSG('.       v_dim is optional and can be ''X'', ''Y'' or ''Z'' if tolerances are different.');
    DashLine;
  END HelpMBR;
  ----------------
  -- HelpTables is detailed help on table procedures.
  -- Syntax:  EXEC HelpTables;
  --
  PROCEDURE HelpTables IS
  BEGIN
    HelpHdr;
    DashLine;
    HelpSyntax;
    TitleBlock('Tables - Table related procedures.');
    DBMSG('PROCEDURE CopyTable(v_srcownertable IN VARCHAR2,');
    DBMSG('.                   v_tgtownertable IN VARCHAR2,');
    DBMSG('.                   b_sidx          IN BOOLEAN DEFAULT TRUE,');
    DBMSG('.                   b_data          IN BOOLEAN DEFAULT TRUE,');
    DBMSG('.                   b_gdosys        IN BOOLEAN DEFAULT TRUE)');
    DBMSG('.');
    DBMSG('CopyTable create a copy of the source table.  Options included spatial index creation,');
    DBMSG('data, and GDOSYS metadata creation.  The default is True, True, True.');
    DBMSG('Syntax: EXEC GOOM.CopyTable(v_scrtable, v_tgttable, b_sidx, b_data, b_gdosys);');
    DBMSG('.       v_scrtable is the source table.');
    DBMSG('.       v_tgttable is the target table.');
    DBMSG('.       b_sidx when TRUE, create the spatial index, default TRUE.');
    DBMSG('.       b_data when TRUE, copy the source data to the targe table, default TRUE.');
    DBMSG('.       b_gdosys when TRUE, create the GDOSYS metadata entries for the target table.');
    DotLine;
    DBMSG('PROCEDURE CreateSequence(v_tablename IN VARCHAR2, v_Column IN VARCHAR2 DEFAULT NULL)');
    DBMSG('.');
    DBMSG('CreateSequence creates a Sequence for specified table/column.  Sequence will start at');
    DBMSG('MAX(col)+1.');
    DBMSG('Syntax: EXEC GOOM.CreateSequence(v_tablename,[v_column]);');
    DBMSG('.       v_tablename is required.');
    DBMSG('.       v_column is optional but is generally the column using the sequence.');
    DotLine;
    DBMSG('PROCEDURE CreateNewSequence(v_tablename IN VARCHAR2, v_Column IN VARCHAR2 DEFAULT NULL, ');
    DBMSG('.                           v_seqname OUT VARCHAR2)');
    DBMSG('.');
    DBMSG('CreateNewSequence will create a new sequence for the specified table and optional  ');
    DBMSG('columnname and will return the sequence name.');
    DBMSG('Syntax: EXEC GOOM.CreateNewSequence(v_tablename,[v_column],v_seqname);');
    DBMSG('.       v_tablename is required.');
    DBMSG('.       v_column is optional but is generally the column using the sequence.');
    DBMSG('.       v_seqname must be declared in the calling procedure.');
    DotLine;
    DBMSG('PROCEDURE DelDupRows(v_tablename IN VARCHAR2, v_column IN VARCHAR2)');
    DBMSG('.');
    DBMSG('DelDupRows deletes duplicate records in a table based on the specified column.');
    DBMSG('EXEC GOOM.DelDupRows(v_tablename, v_column);');
    DBMSG('This procedure is destructive, make sure you understand what it is doing.');
    DotLine;
    DBMSG('PROCEDURE DropTable(v_tablename IN VARCHAR2)');
    DBMSG('.');
    DBMSG('DropTable drops the table and any associated sequences, indexes, Oracle metadata ');
    DBMSG('and GDOSYS metadata.');
    DBMSG('Syntax: EXEC GOOM.DropTable(v_tablename);');
    DotLine;
    DBMSG('PROCEDURE DropSequence(v_sequence IN VARCHAR2)');
    DBMSG('.');
    DBMSG('DropSequence drops the specified sequence if it exists.');
    DBMSG('Syntax: EXEC GOOM.DropSequence(v_sequence);');
    DBMSG('.       v_sequence is the sequence name.');
    DotLine;
    DBMSG('FUNCTION GetSequenceName(v_tablename IN VARCHAR2, v_column IN VARCHAR2 DEFAULT NULL, ');
    DBMSG('.                        b_unique IN BOOLEAN DEFAULT FALSE) RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('GetSequenceName returns a unique sequence name based on the table/column name.');
    DBMSG('Syntax: v_seqname:=GOOM.GetSequenceName(v_tablename,[v_column]);');
    DBMSG('.       v_tablename is required.');
    DBMSG('.       v_column is optional but is generally the column using the sequence.');
    DotLine;
    DBMSG('PROCEDURE AddPrimaryKey(v_table IN VARCHAR2, v_keycolnew IN VARCHAR2 DEFAULT ''PID'',');
    DBMSG('.                       b_dropflag IN BOOLEAN DEFAULT TRUE);');
    DBMSG('.');
    DBMSG('AddPrimaryKey adds a new integer based primary key column to a table, creates a ');
    DBMSG('sequence, then updates the key with sequence values.  You can choose to keep the');
    DBMSG('current key (if it exists) as a normal column or delete it.');
    DBMSG('Syntax: EXEC GOOM.AddPrimaryKey(v_table, [v_keycolnew], [TRUE]);');
    DBMSG('.       v_table is required and can be in the form of OWNER.TABLE.');
    DBMSG('.       v_keycolnew is the optional new column name. PID is default.');
    DBMSG('.       b_dropflag is the optional boolean to drop the current key column if');
    DBMSG('.       it exists.  The default here is FALSE.');
    DashLine;
  END HelpTables;
  ----------------
  -- HelpTUNING is detailed help on spatial indexing and tuning procedures.
  -- Syntax:  EXEC HelpTUNING;
  --
  PROCEDURE HelpTUNING IS
  BEGIN
    HelpHdr;
    DashLine;
    HelpSyntax;
    TitleBlock('TUNING Procedures - Spatial Indexing and Analysis.');
    DashLine;
    DBMSG('PROCEDURE AutoTune(v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('Autotune is a multi-function that will tune a schema that has been loaded using');
    DBMSG('Export to Oracle Object Model data.  The process repairs NULL geometries, sets ');
    DBMSG('default Oracle metadata (MBR), create spatial indexes, and generates statistics.');
    DBMSG('Syntax: EXEC GOOM.Autotune([v_schema]);');
    DBMSG('.       v_schema is optional and can be used by a DBA to operate on any schema.');
    DotLINE;
    DBMSG('PROCEDURE DelSidx(v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('DelSIDX deletes all the spatial indexes in the specified schema.');
    DBMSG('Syntax: EXEC GOOM.DelSIDX; -- for the current schema.');
    DBMSG('.       EXEC GOOM.DelSIDX(v_schema); -- as a DBA operation.');
    DotLINE;
    DBMSG('PROCEDURE DropSidx(v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL)');
    DBMSG('.');
    DBMSG('DropSidx drops the spatial index on the specified table/geometry pair.');
    DBMSG('Syntax: EXEC GOOM.DropSIDX(v_table_name,v_column_name)');
    DotLINE;
    DBMSG('PROCEDURE SpatialIndex(v_tablename IN VARCHAR2,');
    DBMSG('.                        b_geomopt IN BOOLEAN DEFAULT TRUE,');
    DBMSG('.                          b_stats IN BOOLEAN DEFAULT TRUE)');
    DBMSG('.');
    DBMSG('SpatialIndex will create a spatial index on all geometry columns in the specified table.');
    DBMSG('Syntax:  EXEC GOOM.SpatialIndex(v_tablename, [b_geomopt], [b_stats]);');
    DBMSG('.        b_geomopt will create a geometry optimized index by default.  Set to FALSE if');
    DBMSG('.          you do not want the geometry to be optimized.  Optimization constrains the');
    DBMSG('.          GTYPE so no other GTYPE can be introduced into the geometry.');
    DBMSG('.        b_stats is TRUE by default and creates statistics for the new index.  Set to FALSE');
    DBMSG('.          to skip statistics, index creation is faster but index will be slower.');
    DotLINE;
    DBMSG('PROCEDURE SpatialIndexAll(v_schema IN VARCHAR2 DEFAULT USER, ');
    DBMSG('.                        b_geomopt IN BOOLEAN DEFAULT TRUE,');
    DBMSG('.                          b_stats IN BOOLEAN DEFAULT TRUE)');
    DBMSG('.');
    DBMSG('SpatialIndexAll will create a spatial index for all geometry based columns in the schema.');
    DBMSG('A DBA can specify any schema for this operation, otherwise it operates on the current schema.');
    DBMSG('Syntax:  EXEC GOOM.SpatialIndexAll([c_owner],[b_geomopt],[b_stats]);');
    DBMSG('.        b_geomopt will create a geometry optimized index by default.  Set to FALSE if');
    DBMSG('.          you do not want the geometry to be optimized.  Optimization constrains the');
    DBMSG('.          GTYPE so no other GTYPE can be introduced into the geometry.');
    DBMSG('.        b_stats is TRUE by default and creates statistics for the new index.  Set to FALSE');
    DBMSG('.          to skip statistics, index creation is faster but index will be slower.');
    DotLINE;
    DBMSG('PROCEDURE Stats(v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('Stats calculates the statistics for the objects in the current (or specified) schema.');
    DBMSG('Syntax: EXEC GOOM.STATS([v_schema]);');
    DBMSG('.       v_schema is optional and can be used by a DBA to operate on any schema.');
    DotLINE;
    DBMSG('PROCEDURE DELStats(v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('DelStats deletes the database statistics associated with the current (or specified) schema.');
    DBMSG('Syntax:  EXEC GOOM.DelSTATS([v_schema]);');
    DBMSG('.       v_schema is optional and can be used by a DBA to operate on any schema.');
    DotLINE;
    DBMSG('FUNCTION GetGOOMIndexSpace RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('Returns the INDX space currently used by the spatial indexing commands.');
    DBMSG('Example: Select GOOM.GetGOOMIndexSpace from dual;');
    DotLINE;
    DBMSG('PROCEDURE SetGOOMIndexSpace(v_indexspace IN VARCHAR2)');
    DBMSG('.');
    DBMSG('Sets the default spatial indexing tablespace.');
    DBMSG('Syntax: EXEC GOOM.SetGOOMIndexSpace(v_indexspace);');
    DBMSG('.       Set v_indexspace to ''D'' to revert to system default of USERS or INDX');
    DashLine;
  END HelpTUNING;
  ----------------
  -- HelpUtilities is detailed help on miscelaneous procedures.
  -- Syntax:  EXEC HelpUtilities;
  --
  PROCEDURE HelpUtilities IS
  BEGIN
    HelpHdr;
    DashLine;
    HelpSyntax;
    TitleBlock('Utilities - Misc GOOM Utility procedures.');
    DBMSG('PROCEDURE VERSION');
    DBMSG('.');
    DBMSG('Version and date information. PLEASE DO NOT MODIFY!');
    DBMSG('Syntax: EXEC GOOM.version;');
    DotLINE;
    DBMSG('PROCEDURE DBMSG(v_Msg IN VARCHAR2, i_maxlen IN INTEGER DEFAULT 255)');
    DBMSG('.');
    DBMSG('DBMsg is used to send DBMS_OUTPUT messages back to the console.  Messages ');
    DBMSG('require SERVEROUTPUT to be on -> SET SERVEROUTPUT ON ');
    DBMSG('Syntax: EXEC GOOM.DBMSG(c_text, [i_max_length]);');
    DBMSG('.       Input c_text, default max length is 255 characters.');
    DotLINE;
    DBMSG('PROCEDURE Response(v_operation IN VARCHAR2, v_results IN VARCHAR2,');
    DBMSG('.                  i_pad IN INTEGER DEFAULT 30, ');
    DBMSG('.                  b_RtJustify IN BOOLEAN DEFAULT FALSE)');
    DBMSG('.');
    DBMSG('Response uses DBMsg to send a formatted response from a procedure or function.');
    DBMSG('Syntax: EXEC GOOM.Response(v_operation,v_result,[i_pad],[b_rjustify];');
    DBMSG('.       v_operation can be the procedure name or anything else up to 30 characters.');
    DBMSG('.       v_results is the information you want to pass to the user.');
    DBMSG('.       i_pad is the number of spaces between operation and result (default 30).');
    DBMSG('.       b_RtJustify determines if the output should be right justified (default FALSE).');
    DotLINE;
    DBMSG('PROCEDURE DashLine(i_length IN INTEGER DEFAULT 80)');
    DBMSG('.');
    DBMSG('Generate a dashed (--) line of specified length.');
    DBMSG('Syntax:  EXEC GOOM.DashLine([i_length]);');
    DBMSG('.        Default length is 80.');
    DotLINE;
    DBMSG('PROCEDURE DotLine(i_length IN INTEGER DEFAULT 80)');
    DBMSG('.');
    DBMSG('Generate a dotted (...) line of specified length.');
    DBMSG('Syntax:  EXEC GOOM.DotLine([i_length]);');
    DBMSG('.        Default length is 80.');
    DotLINE;
    DBMSG('PROCEDURE DblLine(i_length IN INTEGER DEFAULT 80)');
    DBMSG('.');
    DBMSG('Generate a double (===) line of specified length.');
    DBMSG('Syntax:  EXEC GOOM.DblLine([i_length]);');
    DBMSG('.        Default length is 80.');
    DotLINE;
    DBMSG('PROCEDURE GenLine(v_char IN VARCHAR2, i_length IN INTEGER DEFAULT 80)');
    DBMSG('.');
    DBMSG('Generate a line of any character with a specified length.');
    DBMSG('Syntax:  EXEC GOOM.GenLine(v_char, [i_length]);');
    DBMSG('.        Default length is 80.');
    DotLINE;
    DBMSG('PROCEDURE TitleBlock(v_title IN VARCHAR2, i_length IN INTEGER DEFAULT 80, ');
    DBMSG('.');
    DBMSG('.                    v_sep IN VARCHAR2 DEFAULT ''-'')');
    DBMSG('Generate a formatted title block.');
    DBMSG('Syntax:  EXEC GOOM.TitleBlock(v_title,[i_length], [v_v_sep]);');
    DBMSG('.        c_title is the centered title line (required).');
    DBMSG('.        i_length is the length of the title block, default 80.');
    DBMSG('.        c_separator is the character separator, default is -.');
    DotLINE;
    DBMSG('PROCEDURE TitleLine(v_title IN VARCHAR2, i_length IN INTEGER DEFAULT 80, ');
    DBMSG('.');
    DBMSG('.                   v_sep IN VARCHAR2 DEFAULT ''-'')');
    DBMSG('Generate a title line.');
    DBMSG('Syntax:  EXEC GOOM.TitleLine(v_title,[i_length], [v_v_sep]);');
    DBMSG('.        v_title is the centered title line (required).');
    DBMSG('.        i_length is the length of the title block, default 80.');
    DBMSG('.        v_separator is the character separator, default is -.');
    DotLINE;
    DBMSG('PROCEDURE ProcessStart(v_cmdname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('Indicate that a process has started (used when running a string of procedures).');
    DBMSG('Syntax:  EXEC ProcessStart(v_procname, v_schema);');
    DBMSG('.        v_cmdname is the name of the running process.');
    DBMSG('.        v_schema is the user/schema where process is running.');
    DotLINE;
    DBMSG('PROCEDURE ProcessComplete(v_cmdname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('Indicate that a process has ended (used when running a string of procedures).');
    DBMSG('Syntax:  EXEC ProcessComplete(v_cmdname, v_schema);');
    DBMSG('.        v_cmdname is the name of the running process.');
    DBMSG('.        v_schema is the user/schema where process is running.');
    DotLINE;
    DBMSG('PROCEDURE ProcessTerminate(v_cmdname IN VARCHAR2, v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('Indicate that a group of processes has ended (used when running a string of procedures).');
    DBMSG('Syntax:  EXEC ProcessTerminate(v_cmdname, v_schema);');
    DBMSG('.        v_cmdname is the name of the running process.');
    DBMSG('.        v_schema is the user/schema where process is running.');
    DotLINE;
    DBMSG('PROCEDURE REPORT_ERROR(v_cmdname IN VARCHAR2, v_process IN VARCHAR2, v_debug IN VARCHAR2, ');
    DBMSG('.                      v_SQLCODE IN VARCHAR2, v_SQLERRM IN VARCHAR2)');
    DBMSG('.');
    DBMSG('Output a formatted error message to the end user.');
    DBMSG('Syntax:  EXEC GOOM.REPORT_ERROR (v_cmdname,v_process,v_debug,v_SQLCODE,v_SQLERRM);');
    DBMSG('.        v_cmdname is the name of the procedure/function generating the error.');
    DBMSG('.        v_process is the process being run when error occurred.');
    DBMSG('.        v_debug is a debug msg embedded in the procedure/function.');
    DBMSG('.        v_SQLCODE,v_SQLERRM are system level codes from Oracle.');
    DotLINE;
    DBMSG('PROCEDURE WRITE_RESULTS(v_schema IN VARCHAR2 DEFAULT USER, v_type IN VARCHAR2, ');
    DBMSG('.                       v_cmd IN VARCHAR2, v_feature IN VARCHAR2, v_result IN VARCHAR2)');
    DBMSG('.');
    DBMSG('Write operations and results to the GOOM_LOG table for later review if logging is turned on.');
    DBMSG('Syntax: EXEC GOOM.WRITE_RESULTS(v_type,v_cmd,v_feature,v_result);');
    DBMSG('.       v_type is the category of the process, use to sort log table.');
    DBMSG('.       v_cmd is the procedure or function that is running.');
    DBMSG('.       v_feature is the feature class being processed.');
    DBMSG('.       v_result is the result being logged.');
    DashLine;
  END HelpUtilities;
  ----------------
  -- HelpVALIDATION is detailed help on data validation procedures.
  -- Syntax:  EXEC HelpVALIDATION;
  --
  PROCEDURE HelpVALIDATION IS
  BEGIN
    HelpHdr;
    DashLine;
    HelpSyntax;
    TitleBlock('VALIDATION Procedures - Data Validation and Repair procedures');
    DBMSG('PROCEDURE FixRedundantPoints(v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, ');
    DBMSG('                             n_tol IN NUMBER DEFAULT 0.00005)');
    DBMSG('.');
    DBMSG('FixRedundantPoints uses Oracle''s REMOVE_DUPLICATE_VERTICES to remove redundant points');
    DBMSG('based on the tolerance value.');
    DBMSG('Syntax: EXEC GOOM.FixRedundantPoints(v_table_name,v_geomcol,[n_tol]);');
    DBMSG('        n_tol is optional. If not specified, the default of 0.00005 will be used.');
    DBMSG('Note: Remember that tolerance is based on units of storage.');
    DotLINE;
    DBMSG('PROCEDURE FixSmallArcs(v_tablename IN VARCHAR2,');
    DBMSG('                       v_geomcol   IN VARCHAR2 DEFAULT NULL,');
    DBMSG('                       v_length    IN NUMBER DEFAULT 0.05,');
    DBMSG('                       n_tol       IN NUMBER DEFAULT 0.001)');
    DBMSG('.');
    DBMSG('FixSmallArcs will find and stroke arcs (into line strings) that meet the chord length ');
    DBMSG('and tolerance criteria.  Small arcs can cause problems in both GeoMedia and in Oracle''s');
    DBMSG(' own spatial calculations.');
    DBMSG('Syntax: EXEC GOOM.FixSmallArcs(c_table_name,c_geom_column,[n_chordlength],[n_tolerance]);');
    DBMSG('        n_chordlength is the length of the chord that measures the tolerance. Default 0.05.');
    DBMSG('        n_tol is the distance between the arc and the chord length.  Default 0.001.');
    DBMSG('Note: The defaults are representative of small arcs and work well.  Keep in mind that ');
    DBMSG('      these values are based on the units of storage.');
    DotLINE;
    DBMSG('PROCEDURE FixNullGeoms(v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('FixNullGeom repairs NULL (uninitialized) geometries caused by using SQL Loader.');
    DBMSG('It does this by initializing the geometry column and making it empty.');
    DBMSG('Syntax: EXEC GOOM.FixNullGeom; or EXEC GOOM.FixNullGeom([v_schema]);');
    DBMSG('        v_schema is optional, normally the current schema is used.  A DBA can specify');
    DBMSG('        any schema.');
    DBMSG('Note: This procedure does not harm existing data.');
    DotLINE;
    DBMSG('PROCEDURE FixTrailingSpaces(v_schema IN VARCHAR2 DEFAULT USER)');
    DBMSG('.');
    DBMSG('FixTrainingSpaces removes trailing spaces from all VARCHAR2 columns in a schema.');
    DBMSG('This is sometimes an issue when importing data from Microsoft Access based warehouses.');
    DBMSG('Syntax: EXEC GOOM.FixTrailingSpaces;');
    DBMSG('Note: This procedure does not harm existing data.');
    DotLINE;
    DBMSG('FUNCTION GetError(v_error VARCHAR2 DEFAULT ''EMPTY'') RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('GetError returns a text description of the spatial error code (from ValidateGeom).');
    DBMSG('Syntax: v_RESULT:=GOOM.GetError(v_error);');
    DBMSG('        SELECT GOOM.GetError(v_error) FROM DUAL;');
    DotLINE;
    DBMSG('PROCEDURE RepairGeometry(v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL, ');
    DBMSG('                         n_tol IN NUMBER DEFAULT 0.00005)');
    DBMSG('.');
    DBMSG('RepairGeometry uses Oracle''s Rectify_Geometry to repair the geometry.  Rectify fixes ');
    DBMSG('redundant points (ORA-13356), reversed polygons (ORA-13367), and self-intersecting ');
    DBMSG('polygons (ORA-31349).');
    DBMSG('Syntax: EXEC GOOM.RepairGeometry(v_tablename,v_geomcol, n_tol);');
    DBMSG('        n_tol is optional. If not specified, the default of 0.00005 will be used.');
    DBMSG('Note: Remember that tolerance is based on units of storage.  This procedure does not');
    DBMSG('support owner.table.  Be careful with this one as it may be data destructive.');
    DotLINE;
    DBMSG('PROCEDURE ValidateGeom(v_tablename IN VARCHAR2, v_geomcol IN VARCHAR2 DEFAULT NULL)');
    DBMSG('.');
    DBMSG('ValidateGeom uses Oracle''s VALIDATE_LAYER_WITH_CONTEXT to validate the geometry in');
    DBMSG('the feature class. It handles the error table as well as other bookkeeping.');
    DBMSG('Syntax: EXEC GOOM.ValidateGeom(v_tablename,v_geomcol);');
    DBMSG('        v_tablename CANNOT use OWNER.TABLE.  The owner of the table must runt he command.');
    DBMSG('        v_geomcol is the geometry column to be validated.');
    DotLINE;
    DBMSG('FUNCTION AnalyzeGeometry ( g_geometry IN SDO_GEOMETRY, v_verbose in VARCHAR2 DEFAULT ''FALSE'');');
    DBMSG('.                          RETURN VARCHAR2');
    DBMSG('AnalyzeGeometry looks at a single sdo_geometry object and tests it for validity.  It will');
    DBMSG('return a detailed error message. If an error condition exists, a detailed analysis ');    
    DBMSG('will be returned to the console (if SERVEROUTPUT is ON).  If you want the detailed');
    DBMSG('analysis regardless of an error condition, set the v_vebose flag to TRUE.');
    DBMSG('Syntax: SELECT GOOM.AnalyzeGeometry(g_geometry) FROM table WHERE PKID=val;');
    DashLine;
  END HelpVALIDATION;
  ----------------
  -- HelpFUNCTIONS is detailed help on misc functions.
  -- Syntax:  EXEC HelpFUNCTIONS;
  --
  PROCEDURE HelpFUNCTIONS IS
  BEGIN
    HelpHdr;
    DashLine;
    HelpSyntax;   
    TitleBlock('Utility FUNCTIONS: Return an object name or value');
    DotLINE;
    DBMSG('FUNCTION Ang2Rad ( n_degrees IN NUMBER) RETURN NUMBER;');
    DBMSG('.');
    DBMSG('Returns the input degrees to radians.');
    DBMSG('Syntax: n_radians := Ang2Rad(n_degrees);');
    DotLINE;
    DBMSG('FUNCTION Rad2Ang ( n_radians IN NUMBER) RETURN NUMBER;');
    DBMSG('.');
    DBMSG('Returns the input radians to degrees.');
    DBMSG('Syntax: n_degrees := Rad2Ang(n_radians);');
    DotLINE;
    DBMSG('FUNCTION ROTINDEX ( v_type IN VARCHAR2, n_degrees IN NUMBER) RETURN NUMBER; ');
    DBMSG('.');
    DBMSG('Returns the rotation matirx index value for the input angle in degrees.');
    DBMSG('Syntax: n_irot := ROTINDEX(''I'',n_degrees);');
    DBMSG('.       n_jrot := ROTINDEX(''J'',n_degrees);');
    DotLINE;
    DBMSG('FUNCTION ROTANGLE ( n_irot IN NUMBER, n_jrot IN NUMBER) RETURN NUMBER;');
    DBMSG('.');
    DBMSG('Returns the rotation angle in degrees from the input i, j rotation indicies.');
    DBMSG('Syntax: n_degrees := ROTANGLE(n_irot, n_jrot);');
    DotLINE;   
    DBMSG('FUNCTION DMS2DD(v_dms IN VARCHAR2) RETURN NUMBER');
    DBMSG('.');
    DBMSG('Returns Degree:Minute:Seconds from Decimal Degrees.');
    DBMSG('Syntax: n_dec_degrees := DMS2DD(v_dms);');
    DBMSG('.       SELECT DMS2DD(''40:20:50'') FROM DUAL;');
    DBMSG('.       v_dms is colon (:) delimited by default.');
    DotLINE;
    DBMSG('FUNCTION DD2DMS(n_dd IN NUMBER, v_delim IN VARCHAR2 DEFAULT '':'') RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('Returns Decimal Degrees from Lat or Lon: Degree, Minute, Seconds using delimiter.');
    DBMSG('Syntax: v_dec_degrees := DMS2DD(v_dms,v_delim);');
    DBMSG('.       v_dms := DD2DMS(n_dd,v_delim);');
    DBMSG('.       SELECT DD2DMS(40.3472222,'':'') FROM DUAL;');
    DBMSG('.       v_delim can be any single character like '':''');
    DotLINE;
    DBMSG('FUNCTION RandInRange(i_lo, i_hi) RETURN NUMBER;');
    DBMSG('.');
    DBMSG('Returns a random real number between the integer values i_lo and i_hi.');
    DBMSG('Syntax: n_value:=GOOM.RandInRange(i_lo, i_hi);');
    DotLINE;
    DBMSG('FUNCTION Integer2Text(i_integer IN INTEGER) RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('Returns 4 bytes of text from the input integer.');
    DBMSG('Syntax: v_text:=GOOM.Integer2Text(i_integer);');
    DotLine;
    DBMSG('FUNCTION Text2Integer(v_string IN VARCHAR2) RETURN INTEGER');
    DBMSG('.');
    DBMSG('Returns an integer value from 4 bytes of text.  Anything more will be truncated.');
    DBMSG('Syntax: i_integer:=GOOM.Text2Integer(v_string);');
    DotLINE;
    DBMSG('FUNCTION Integers2String(v_string IN VARCHAR2) RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('Returns a text string from a comma delimited string of integers.');
    DBMSG('Syntax: v_string:=GOOM.Integers2String(v_integerList);');
    DotLINE;
    DBMSG('FUNCTION String2Integers(v_string IN VARCHAR2) RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('Returns a comma delimited string of integers, every 4 bytes (incl. spaces) ');
    DBMSG('is represented by one integer.');
    DBMSG('Syntax: i_integerList:=GOOM.String2Integers(v_string);');
    DotLINE;
    DBMSG('FUNCTION SplitOwnerObject(v_tablename IN VARCHAR2, v_type IN VARCHAR2) RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('Returns either the Owner name or the Object name from the owner.object format.');
    DBMSG('Syntax: v_schema:=GOOM.SplitOwnerObject(v_tablename,v_type);');
    DBMSG('        v_schema:=GOOM.SplitOwnerObject(v_tablename,''OWNER'');');
    DBMSG('        v_table:=GOOM.SplitOwnerObject(v_tablename,''OBJECT'');');
    DBMSG('.  Valid c_types are OWNER/USER or TABLE/OBJECT (use TABLE for views).');
    DashLine;       
    TitleLine('GET FUNCTIONS: Return an object name or value');
    DotLine;    
    DBMSG('FUNCTION GetDBNAME RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('Returns the global database name.');
    DBMSG('Syntax: Select GOOM.GetDBNAME from DUAL;');
    DotLINE;
    DBMSG('FUNCTION GETDBVERSION RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('Returns the Database Version number.');
    DBMSG('Syntax: Select GOOM.GetDBVersion from DUAL;');
    DotLine;  
    DBMSG('FUNCTION GetOwnerObject(v_tablename) RETURN VARCHAR2;');  
    DBMSG('.');
    DBMSG('Returns the tablename in the format or OWNER.TABLE');      
    DBMSG('Syntax: v_ownertable:=GOOM.GetOwnerObject(v_tablename);');        
    DotLine; 
    DBMSG('FUNCTION GetDim( v_tablename,v_geometry ) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns the dim of a layer - 2 or 3 as VARCHAR2(4)');
    DBMSG('Syntax: v_dim:=GOOM.GetDim(v_tablename,v_geometry);');
    DotLine; 
    DBMSG('FUNCTION GetGeom(v_tablename) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns the 1st geometry column for a layer as VARCHAR2');
    DBMSG('Syntax: g_geometry:=GOOM.GetGeom(v_tablename);');
    DotLine; 
    DBMSG('FUNCTION GetSRID( v_tablename,v_geometry) RETURN INTEGER;');
    DBMSG('.');
    DBMSG('Returns the 1st srid found or NULL.');
    DBMSG('Syntax: i_srid:=GOOM.GetSRID(v_tablename,v_geometry);');
    DotLine; 
    DBMSG('FUNCTION GetCSName( i_srid ) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns the CS name of the provided SRID.');
    DBMSG('Syntax: v_csname:=GOOM.GetSRID(i_srid);');
    DotLine; 
    DBMSG('FUNCTION isGeographic (i_srid IN INTEGER DEFAULT 0) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('If the SRID is a geographic srid then do something.');
    DBMSG('Syntax: If isGeographic THEN');
    DotLine; 
    DBMSG('FUNCTION GetINDXNAME(v_tablename, v_extend ) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns a unique index name based on tablename VARCHAR2(30)');
    DBMSG('and a 3 character extension.  The default extension is _SI ');
    DBMSG('Syntax: v_index:=GOOM.GetINDXNAME(v_tablename, v_extend);');
    DotLine; 
    DBMSG('FUNCTION GetSequenceName (v_tablename , v_column, b_unique) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns sequence name based on input table and column.');
    DBMSG('Syntax: v_seqname:=GOOM.GetSequenceName(v_tablename,v_column,v_unique);');
    DBMSG('v_column is optional and the default is NULL.');
    DBMSG('b_unique is optional and the default is FALSE, if TRUE, a unique name will be generated.');
    DotLine; 
    DBMSG('FUNCTION GetGTYPE(v_tablename, v_geomcol) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns the GTYPE as VARCHAR2(4)');
    DBMSG('Syntax: v_GTYPE:=GOOM.GetGTYPE(v_tablename,v_geomcol);');
    DotLine; 
    DBMSG('FUNCTION GetGeomType(v_tablename, v_geomcol) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns the GeomType as VARCHAR2(16)');
    DBMSG('Syntax: v_GeomType:=GOOM.GetGeomType(v_tablename,v_geomcol);');
    DBMSG('.    Possible types are POINT, LINE, POLYGON, COLLECTION, MULTIPOINT, MULTILINE,');
    DBMSG('.    MULTIPOLYGON and UNKNOWN.');
    DotLine; 
    DBMSG('FUNCTION GetGDOTableName (v_tabletype) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns the GDOSYS.tablename for the given geomedia gdo tabletype.');
    DBMSG('Syntax: v_gdotable:=GOOM.GetGDOTableName(v_tabletype);');
    DBMSG('Valid v_tabletype values are:');
    DBMSG(' ''INGRFieldLookup'',''GOracleFieldMapping'',''INGRAttributeProperties''');
    DBMSG(' ''INGRGeometryProperties'',''INGRFeatures'',''INGRPickLists''');
    DBMSG(' ''GCoordSystemTable'',''GOracleIndexColumns'',''GParameters''');
    DotLine; 
    DBMSG('FUNCTION GetCount(v_tablename) RETURN INTEGER;');
    DBMSG('.');
    DBMSG('Returns the Row count as an INTEGER');
    DBMSG('Syntax: i_count:=GOOM.GetCount(v_tablename);');
    DotLine; 
    DBMSG('FUNCTION GetTableSize(v_tablename) RETURN NUMBER;');
    DBMSG('.');
    DBMSG('Returns the segment size for the specified table in MB');
    DBMSG('Syntax: n_size:=GOOM.GetTableSize(v_tablename);');
    DotLine; 
    DBMSG('FUNCTION GetError(v_error) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns a text description of spatial error codes as VARCHAR2(255)');
    DBMSG('Syntax: v_RESULT:=GOOM.GetError(v_error);');
    DotLine;
    DBMSG('FUNCTION GetKeyCol(v_tablename) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns the primary key column of a table assuming it only has one.');
    DBMSG('.  Returns NO_KEY if a key does not exist or there is more than 1.');
    DBMSG('Syntax: v_keycol:=GOOM.GetKeyCol(v_tablename);');
    DotLine; 
    DBMSG('FUNCTION GetViewKeyCol(v_viewname) RETURN VARCHAR2;');
    DBMSG('.');
    DBMSG('Returns the primary key column of a view assined in GDOSYS.');
    DBMSG('Returns ''NO_KEY'' if a key does not exist or there is more than 1.');
    DBMSG('Syntax: v_keycol:=GOOM.GetViewKeyCol(v_viewname);');
    DotLine; 
    DBMSG('FUNCTION GetColumnType (v_tablename, v_keycol) RETURN VARCHAR2');
    DBMSG('.');
    DBMSG('Returns the data type for the specified primary key.');    
    DBMSG('Syntax: v_keytype:=GOOM.GetColumnType(v_tablename,v_keycol);');    
    DotLine; 
    DBMSG('FUNCTION GOOM.GetPrimaryGeom(v_tablename) RETURN VARCHAR2'); 
    DBMSG('.');
    DBMSG('Returns the primary geometry used by GeoMedia when a FC ');
    DBMSG('contains more than one geometry field. ');       
    DBMSG('Syntax: v_primarygeom:=GOOM.GetPrimaryGeom(v_tablename); ');
    TitleBlock('CHECK FUNCTIONS: Return BOOLEAN if condition exists');
    DBMSG('FUNCTION chkTable(v_tablename) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if table exists otherwise FALSE as BOOLEAN');
    DBMSG('Syntax: IF GOOM.ChkTable(v_tablename) THEN');
    DotLine;
    DBMSG('FUNCTION chkView(v_tablename) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if this is a view otherwise FALSE as BOOLEAN');
    DBMSG('Syntax: IF GOOM.ChkView(v_tablename) THEN');
    DotLine;
    DBMSG('FUNCTION chkTrigger(v_trigname) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if trigger exists otherwise FALSE as BOOLEAN');
    DBMSG('Syntax: IF GOOM.chkTrigger(v_trigname) THEN');
    DotLine;     
    DBMSG('FUNCTION chkGeometry(v_tablename, v_geomcol IN VARCHAR2) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if this is a geometry otherwise FALSE as BOOLEAN');
    DBMSG('Syntax: IF GOOM.chkGeometry(v_tablename,v_geomcol) THEN');
    DotLine;
    DBMSG('FUNCTION chkSequence(v_seqname) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if sequence exists otherwise FALSE as BOOLEAN');
    DBMSG('Syntax: IF GOOM.chkSequence(v_seqname) THEN');
    DotLine; 
    DBMSG('FUNCTION chkIndex(v_indexname) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if index exists otherwise FALSE as BOOLEAN');
    DBMSG('Syntax: IF GOOM.chkIndex(v_indexname) THEN');
    DotLine;         
    DBMSG('FUNCTION chkSpatialIndex(v_tablename, v_geomcol) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if spatial index exists otherwise FALSE as BOOLEAN');
    DBMSG('Syntax: IF GOOM.chkSptaialIndex(v_tablename,v_geomcol) THEN');
    DotLine; 
    DBMSG('FUNCTION chkMetadata(v_tablename) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if metadata exists otherwise FALSE as BOOLEAN.');
    DBMSG('Syntax: IF GOOM.ChkMetadata(v_tablename) THEN');
    DotLine; 
    DBMSG('FUNCTION ChkMBR(v_tablename, v_geomcol) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if Oracle metadata exists otherwise FALSE as BOOLEAN.');
    DBMSG('Syntax: IF GOOM.ChkMBR(v_tablename,v_geomcol) THEN ');
    DotLine; 
    DBMSG('FUNCTION chkPKEY(v_tablename,v_column) RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if column is a key column otherwise FALSE as BOOLEAN');
    DBMSG('Syntax: IF GOOM.chkPKEY(v_tablename,v_column) THEN');
    DotLine; 
    DBMSG('FUNCTION chkInsertOnMDSYS RETURN BOOLEAN;');
    DBMSG('.');
    DBMSG('Returns TRUE if user has insert privileges on MDSYS.SDO_GEOM_METADATA_TABLE, ');
    DBMSG('otherwise FALSE. If TRUE, a user can insert Oracle metadata for a table they ');
    DBMSG('do not own but have privileges on.');
    DBMSG('Syntax: IF GOOM.chkInsertOnMDSYS THEN');
    DblLine;
  END HelpFUNCTIONS;
  --
  PROCEDURE HELPME IS
  BEGIN
    HelpHdr;
    DashLine;
    HelpSyntax;
    DashLine;
    TitleLine('Most Common Procedures and Functions');
    DashLine;
    TitleLine('MBR (Oracle Metadata)');
    DotLine;
    DBMSG('EXEC GOOM.CopyMBR(v_source_table,v_source_geom,v_target_table,v_target_geom);');
    DBMSG('EXEC GOOM.DeleteOrphanMBR([v_owner]);');
    DBMSG('. ');
    DBMSG('EXEC GOOM.SetMBR (v_tablename,[v_geometry_col],[i_dim_override]);');
    DBMSG('EXEC GOOM.SetMBRAll;  or EXEC GOOM.SetMBRAll(v_owner,[i_dim_override]);');
    DBMSG('EXEC GOOM.SetMBRgeo;  or EXEC SetMBRgeo(v_tablename);');
    DBMSG('EXEC GOOM.SetMBRproj; or EXECSetMBRproj(v_tablename);');
    DBMSG('. ');
    DBMSG('EXEC GOOM.SetSRID(v_tablename,v_geometry_col,i_srid);');
    DBMSG('EXEC GOOM.SetSRIDAll(i_srid, [v_owner]);');
    DBMSG('. ');
    DBMSG('EXEC GOOM.SetSpatialTolerance(v_tablename,v_geometry_col,n_tol); ');
    DBMSG('EXEC GOOM.SetSpatialToleranceAll(n_tol,[v_owner]);');
    DBMSG('SELECT GOOM.GetSpatialTolerance(v_tablename,v_geometry_col) FROM DUAL; ');
    DotLine;
    TitleLine('GDOSYS (GeoMedia Metadata)');
    DotLine;
    DBMSG('EXEC GOOM.ClearModLog;');
    DBMSG('EXEC GOOM.LogDataModification(v_tablename, v_keyname, v_keyvalue, i_modtype);');
    DBMSG('. ');
    DBMSG('EXEC GOOM.DeleteOrphanCS;  ');
    DBMSG('EXEC GOOM.DelGDOSYSOrphans; or EXEC GOOM.DelGDOSYSOrphans(v_owner);' );
    DBMSG('EXEC GOOM.DBADelGDOSYSOrphans;');
    DBMSG('EXEC GOOM.DelGDOSYSMetadata(v_tablename);');
    DBMSG('EXEC GOOM.SetGDOSYSMetadata(v_tablename,[v_seqname],[v_cs],[i_gtype_in],');
    DBMSG('-                           [v_keycolumn],[v_num38],[v_num10]);');
    DBMSG('EXEC GOOM.SetGDOSYSMetadataAll; or EXEC GOOM.SetGDOSYSMetadataAll(v_owner);');  
    DBMSG('. ');  
    DBMSG('EXEC GOOM.SetDefaultCS(v_csguid, v_owner);');
    DBMSG('EXEC GOOM.SetField2Hypertext(v_tablename, v_column);');
    DBMSG('EXEC GOOM.SetPrimaryGeom(v_tablename, v_geomcol);'); 
    DBMSG('EXEC GOOM.OverrideNumDatatype(v_tablename, v_column, v_type);' );  
    DotLine;
    TitleLine('SPATIAL TUNING (Indexing)');
    DotLine;
    DBMSG('EXEC GOOM.AutoTune([v_owner]);');
    DBMSG('EXEC GOOM.DelSidx([v_owner]);');
    DBMSG('EXEC GOOM.DropSIDX(v_ownertable,v_geometry_col);  ');
    DBMSG('EXEC GOOM.SpatialIndex(v_ownertable,[b_opt],[b_stats]); ');
    DBMSG('EXEC GOOM.SpatialIndexAll([v_owner],[b_opt],[b_stats]); ');
    DBMSG('. ');
    DBMSG('EXEC GOOM.STATS([v_schema]);'); 
    DBMSG('EXEC GOOM.DelSTATS([v_schema]);');
    DBMSG('. ');   
    DBMSG('EXEC GOOM.SetGOOMIndexSpace(v_indexspace);');
    DBMSG('Select GOOM.GetGOOMIndexSpace from dual;');
    DotLine;
    TitleLine('GEOMETRY VALIDATION');
    DotLine;
    DBMSG('EXEC GOOM.ValidateGeom(v_table_name,v_geometry_col);');
    DBMSG('EXEC GOOM.RepairGeometry(v_table_name,v_geometry_col,[n_tolerance])');
    DBMSG('EXEC GOOM.FixNullGeoms; or  EXEC GOOM.FixNullGeoms(v_owner);     ');
    DBMSG('EXEC GOOM.FixRedundantPoints(v_table_name,v_geometry_col,[n_tolerance])');
    DBMSG('EXEC GOOM.FixSmallArcs(v_table_name,v_geometry_col,[n_length],[n_tolerance]));');
    DBMSG('EXEC GOOM.FixTrailingSpaces;  or EXEC GOOM.FixTrailingSpaces(v_owner);');
    DBMSG('SELECT GOOM.AnalyzeGeometry( g_geometry ) FROM tablename WHERE ROWNUM=1;');
    DotLine;
    TitleLine('MISC Procedures and Functions');
    DotLine;
    DBMSG('EXEC GOOM.VERSION; ');
    DBMSG('EXEC GOOM.WRITE_RESULTS(v_type,v_command,v_feature,v_result);');
    DBMSG('EXEC GOOM.REPORT_ERROR( v_command,v_feature,v_debug,v_SQLCODE,v_SQLERRM);');
    DBMSG('EXEC GOOM.DBMSG(''Text'');');
    DBMSG('EXEC GOOM.RESPONSE(''Operation'',''Result'');');
    DBMSG('. ');
    DBMSG('EXEC GOOM.CopyTable(v_scrtable, v_tgttable, [b_sidx], [b_data], [b_gdosys]);          ');
    DBMSG('EXEC GOOM.DropTable(v_tablename);');
    DBMSG('. ');
    DBMSG('EXEC GOOM.AddPrimaryKey(v_tablename,[v_newkeycol],[b_dropkey])');
    DBMSG('EXEC GOOM.CreateNewSequence(v_table_name,[v_col],v_seqname); (v_seqname is returned)');
    DBMSG('SELECT GOOM.GetSequenceName(v_table_name,[v_col],[v_b_uniq];');
    DBMSG('EXEC GOOM.DropSequence(v_Sequence_name);');
    DBMSG('. ');
    DBMSG('EXEC GOOM.DelDupRows(v_tablename, v_column);          '); 
    DBMSG('. ');       
    DBMSG('EXEC GOOM.SetGOOMIndexSpace(v_INDX_SPACE);');
    DBMSG('Select GOOM.GetGOOMIndexSpace from dual;');
    DBMSG('. ');
    DBMSG('v_string:=GOOM.Integers2String(v_integerlist);');
    DBMSG('v_integerList:=GOOM.String2Integers(v_string);');
    DotLine;
    TitleLine('DATA MANIPULATION Functions');
    DotLine;
    DBMSG('SELECT GOOM.ChangeOrdinatePrecision(A.<geometry>, i_dec) FROM <table> A;'); 
    DBMSG('SELECT GOOM.Convert2D(A.<3Dgeometry>) from <table> A;');
    DBMSG('SELECT GOOM.Convert3D(A.<2Dgeometry>, zVal) from <table> A;');
    DBMSG('SELECT GOOM.Convert2OrientedPt(A.<geometry>) from <table> A;'); 
    DBMSG('SELECT GOOM.Convert2NativePt (A.<geometry>,[i_dim]) from <table> A;');
    DBMSG('SELECT GOOM.ConvGeom2Pts(A.<poly_geometry>)  FROM <table> A;');
    DBMSG('. '); 
    DBMSG('SELECT GOOM.DMS2DD(''deg:min:sec'') FROM DUAL'); 
    DBMSG('SELECT GOOM.GetPtCoord(A.<geometry>,v_coord_type) FROM <table> A;');
    DBMSG('SELECT GOOM.GetLinearCoord(A.<geometry>,v_coord_type,''START'') FROM <table> A;');
    DBMSG('. '); 
    DBMSG('SELECT GOOM.Bearing(x1,y1,x2,y2,v_ctype,v_rtype) FROM DUAL;');
    DBMSG('SELECT GOOM.GetPtBearing(g_geom1,g_geom2, v_rtype) FROM DUAL'); 
    DBMSG('SELECT GOOM.GetLinearBearing(A.<geometry>, v_rtype) FROM <table> A;'); 
    DBMSG('SELECT GOOM.GetPointRotation(A.<geometry>, v_type) FROM <table> A;'); 
    DBMSG('SELECT GOOM.SetPointRotation(A.<geometry>, n_degrees) FROM <table> A;'); 
    DblLine;
  END HELPME;
  ----------------------------------------------------------------------------------------
END GOOM;
/
-- -------------------------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------------------------
-- Grant privileges and create PUBLIC synonym for package.
-- -------------------------------------------------------------------------------------------------
SET SERVEROUTPUT ON;
SHOW ERRORS
PROMPT **..............................................................**;
PROMPT ** NOTE: Any errors occurring above may leave package unusable. **;
SET TERMOUT OFF
GRANT EXECUTE ON GDOSYS.GOOM TO PUBLIC;
SET TERMOUT ON
PROMPT **       Execute Privileges have been granted to PUBLIC!        **;
SET TERMOUT OFF
DROP PUBLIC SYNONYM GOOM;
CREATE PUBLIC SYNONYM GOOM FOR GDOSYS.GOOM;
SET TERMOUT ON
PROMPT **       A PUBLIC synonym for GOOM has been created.            **;
PROMPT **..............................................................**;
PROMPT ** To turn on procedural messages, run:  SET SERVEROUTPUT ON    **;  
PROMPT ** For version information, run:         EXEC GOOM.VERSION      **;
PROMPT ** For Online Help Info, run:            EXEC GOOM.HELPME       **;                                                  
PROMPT ** ***************   Installation completed.  ***************** **;
PROMPT ******************************************************************;
-------------------------------------------------------------------------------------------
EXEC GOOM.VERSION;
--
