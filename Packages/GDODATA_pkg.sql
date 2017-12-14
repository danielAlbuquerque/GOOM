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
-- GDODATA_pkg.sql: Generate random values for coordinates, strings, dates,
-- and geometries.  Used to generate spatial data in Oracle 10G/11G  Also includes some useful math
-- functions.
--
-- Run script as system DBA after the creation of the GDOSYS metadata schema.  
-- The GDOBKP package is loaded into GDOSYS and is accessed via a GDOBKP Public 
-- synonym.  The GOOM package is required in order to run this package.  Any user 
-- account that makes use  of this package must be explicitly granted the following 
-- privileges:
-- grant create table to <user>;
-- grant create sequence to <user>;
-- ----------------------------------------------------------------------------------------
-- Release History:
-- 03/21/2008  Released to WEB.
-- 04/10/2009  Released to WEB.
-- 12/03/2009  Released to WEB.
-- 04/30/2010  Released to WEB.
-- 03/15/2012  Released to WEB.
-- 07/15/2013  Released to WEB.
-- 07/15/2014  Released to WEB.
-- 07/15/2015  Released to WEB.
-- 07/15/2015  Released to WEB.
--
-- History (since last release):
-- 07/15/2016   Package frozen, no further enhancements.
--
-- ----------------------------------------------------------------------------------------
SET SERVEROUTPUT ON
SET TERMOUT ON
PROMPT ******************************************************************;
PROMPT **                    GDODATA PACKAGE 2016                      **;
PROMPT **     GeoMedia Oracle Spatial Data Generator Installation      **;
PROMPT **                                                              **;
PROMPT ** This package provides procedures to generate random spatial  **;
PROMPT ** and attribute data in an Oracle schema. This includes table  **;
PROMPT ** generation and population of attribute values including      **;
PROMPT ** columns of type SDO_GEOMETRY.   Spatial data includes        **;
PROMPT ** 2D/3D point data, 2D/3D linear data, 2D polygons, and other  **;
PROMPT ** special geometry types.                                      **;
PROMPT **..............................................................**;
PROMPT **                  INSTALLATION INFORMATION                    **;
PROMPT ** -> Oracle 11G or later is REQUIRED.                          **;
PROMPT ** -> Oracle spatial or locator must be installed.              **;
PROMPT ** -> Installation requires a DBA connection.                   **;
PROMPT ** -> The default installation location is the GDOSYS schema.   **;
PROMPT ** -> The GOOM Package is required and must be installed first. **;
PROMPT **..............................................................**;
--
CREATE OR REPLACE PACKAGE GDOSYS.GDODATA AUTHID CURRENT_USER IS
  TYPE NUM_ARRAY IS TABLE OF NUMBER INDEX BY BINARY_INTEGER;
  -- MISC Function
  PROCEDURE VERSION;
  PROCEDURE INIT (v_seed IN VARCHAR2);
  PROCEDURE HELPME(v_help IN VARCHAR2 DEFAULT NULL);
  PROCEDURE ShowASCII;
  -- Internal Functions
  FUNCTION CreateTrigName (v_tablename IN VARCHAR2) RETURN VARCHAR2;
  -- Random Number functions
  FUNCTION VAL RETURN NUMBER;
  FUNCTION VAL (v_LOW IN NUMBER, v_HIGH IN NUMBER) RETURN NUMBER;
  FUNCTION NORM RETURN NUMBER;
  -- Random Character functions
  FUNCTION STRING (v_OPT CHAR DEFAULT'X', v_LEN NUMBER DEFAULT 32) RETURN VARCHAR2;
  FUNCTION RDATE (v_days IN NUMBER) RETURN DATE;
  -- Angle Functions
  FUNCTION PI RETURN NUMBER;
  -- Linear Functions
  FUNCTION LineLength (v_X1 IN NUMBER, v_Y1 IN NUMBER, v_X2 IN NUMBER, v_Y2 IN NUMBER) RETURN NUMBER;
  FUNCTION GetArcRadius (v_X1 in NUMBER, v_Y1 in NUMBER,
                         v_X2 in NUMBER, v_Y2 in NUMBER,
                         v_X3 in NUMBER, v_Y3 in NUMBER) 
                         RETURN NUMBER;
  -- Other Math Functions
  FUNCTION FAC (v_num POSITIVE) RETURN INTEGER;
  FUNCTION GCD (v_intA IN INTEGER,v_intB IN INTEGER) RETURN INTEGER;
  FUNCTION DIFF (X1 IN NUMBER DEFAULT 1, X2 IN NUMBER DEFAULT 1) RETURN NUMBER;
  -- Random Geometry functions
  FUNCTION RANDOM_PT(v_xlo IN NUMBER   DEFAULT 0, 
                     v_xhi IN NUMBER   DEFAULT 100000, 
                     v_ylo IN NUMBER   DEFAULT 0, 
                     v_yhi IN NUMBER   DEFAULT 100000,
                     v_rot IN NUMBER   DEFAULT NULL,
                     v_dim IN NUMBER   DEFAULT 3,
                     v_typ IN VARCHAR2 DEFAULT 'O') 
                     RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
  --
  FUNCTION RANDOM_LINE(v_xlo    IN NUMBER DEFAULT 0, 
                       v_xhi    IN NUMBER DEFAULT 100000, 
                       v_ylo    IN NUMBER DEFAULT 0, 
                       v_yhi    IN NUMBER DEFAULT 100000, 
                       v_varlo  IN NUMBER DEFAULT 250, 
                       v_varhi  IN NUMBER DEFAULT 250,
                       v_maxver IN NUMBER DEFAULT 25,
                       v_dim    IN NUMBER DEFAULT 3) 
                       RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
  --
  FUNCTION RANDOM_POLY(v_scale IN INTEGER DEFAULT 10, 
                       v_dim   IN INTEGER DEFAULT 3, 
                       v_Xlo   IN NUMBER  DEFAULT 0, 
                       v_Xhi   IN NUMBER  DEFAULT 100000,
                       v_ylo   IN NUMBER  DEFAULT 0,
                       v_yhi   IN NUMBER  DEFAULT 100000,
                       v_typ   IN INTEGER DEFAULT 1) 
                       RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
  --
  FUNCTION LORENZ(v_scale     IN NUMBER DEFAULT 10,
                  v_iteration IN NUMBER DEFAULT 800,
                  v_xorigin   IN NUMBER DEFAULT 0,
                  v_yorigin   IN NUMBER DEFAULT 0 )
                  RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
  --
  FUNCTION SERPENSKI(v_scale     IN NUMBER DEFAULT 1,
                     v_iteration IN NUMBER DEFAULT 400,
                     v_xorigin   IN NUMBER DEFAULT 0,
                     v_yorigin   IN NUMBER DEFAULT 0 )
                     RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
  --
  FUNCTION SPIROGRAPH(v_scale   IN NUMBER         DEFAULT 10,
                      v_res     IN NUMBER         DEFAULT 10,
                      v_cycles  IN NUMBER         DEFAULT 2,
                      v_Aradius IN NUMBER         DEFAULT 100,
                      v_Bradius IN NUMBER         DEFAULT 20,
                      v_param   IN NUMBER         DEFAULT 80,
                      v_xorigin IN NUMBER         DEFAULT 0,
                      v_yorigin IN NUMBER         DEFAULT 0)
                      RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
  -- Other Functions
  FUNCTION CREATE_POLY(v_poly_type IN INTEGER, 
                       v_scale     IN INTEGER, 
                       v_Xorigin   IN NUMBER DEFAULT NULL, 
                       v_Yorigin   IN NUMBER DEFAULT NULL) 
                       RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;
  -- Table Creation Procedures
  PROCEDURE CreateTestTable (v_tablename IN VARCHAR2, 
                               v_tabtype IN VARCHAR2 DEFAULT 'L',
                               v_keytype IN VARCHAR2 DEFAULT 'I',
                              v_geomtype IN VARCHAR2 DEFAULT NULL);
  PROCEDURE Insert_TestTable (v_tablename IN VARCHAR2, 
                               v_tabtype IN VARCHAR2 DEFAULT 'L',
                               v_keytype IN VARCHAR2 DEFAULT 'I',
                              v_comments IN VARCHAR2 DEFAULT 'Simple Default table',
                                  v_geom IN MDSYS.SDO_GEOMETRY DEFAULT NULL);
  --  Auto Data Generation Procedures
  PROCEDURE GenerateAttributeTable(v_tablename IN VARCHAR2,
                                 v_rows      IN INTEGER  DEFAULT 500,
                                 v_tabtype   IN VARCHAR2 DEFAULT 'A',
                                 v_keytype   IN VARCHAR2 DEFAULT 'I');
  --
  PROCEDURE GeneratePtTable(v_tablename IN VARCHAR2,
                            v_rows      IN INTEGER  DEFAULT 500,
                            v_tabtype   IN VARCHAR2 DEFAULT 'L',
                            v_keytype   IN VARCHAR2 DEFAULT 'I',
                            v_type      IN VARCHAR2 DEFAULT 'O',
                            v_xlo       IN NUMBER   DEFAULT 0,
                            v_ylo       IN NUMBER   DEFAULT 0,
                            v_xhi       IN NUMBER   DEFAULT 100000,
                            v_yhi       IN NUMBER   DEFAULT 100000,
                            v_dim       IN INTEGER  DEFAULT 3);
  --
  PROCEDURE GeneratePolyTable(v_tablename IN VARCHAR2,
                              v_rows      IN INTEGER  DEFAULT 500,
                              v_tabtype   IN VARCHAR2 DEFAULT 'L',
                              v_keytype   IN VARCHAR2 DEFAULT 'I',
                              v_xlo       IN NUMBER   DEFAULT 0,
                              v_ylo       IN NUMBER   DEFAULT 0,
                              v_xhi       IN NUMBER   DEFAULT 100000,
                              v_yhi       IN NUMBER   DEFAULT 100000,
                              v_scalelo   IN INTEGER  DEFAULT 10,
                              v_scalehi   IN INTEGER  DEFAULT 20,
                              v_dim       IN INTEGER  DEFAULT 2,
                              v_polytyp   IN INTEGER  DEFAULT 1);
--
  PROCEDURE GenerateLineTable(v_tablename IN VARCHAR2,
                              v_rows      IN VARCHAR2 DEFAULT 500,
                              v_tabtype   IN VARCHAR2 DEFAULT 'L',
                              v_keytype   IN VARCHAR2 DEFAULT 'I',
                              v_maxver    IN VARCHAR2 DEFAULT 25,
                              v_varhi     IN INTEGER  DEFAULT 100,
                              v_xlo       IN NUMBER   DEFAULT 0,
                              v_ylo       IN NUMBER   DEFAULT 0,
                              v_xhi       IN NUMBER   DEFAULT 100000,
                              v_yhi       IN NUMBER   DEFAULT 100000,
                              v_dim       IN INTEGER  DEFAULT 2);
--
  PROCEDURE GenerateCircleTable(v_tablename IN VARCHAR2,
                                v_rows      IN INTEGER  DEFAULT 500,
                                v_tabtype   IN VARCHAR2 DEFAULT 'L',
                                v_keytype   IN VARCHAR2 DEFAULT 'I',
                                v_xlo       IN NUMBER   DEFAULT 0,
                                v_ylo       IN NUMBER   DEFAULT 0,
                                v_xhi       IN NUMBER   DEFAULT 100000,
                                v_yhi       IN NUMBER   DEFAULT 100000,
                                v_diamlo    IN NUMBER   DEFAULT 200,
                                v_diamhi    IN NUMBER   DEFAULT 1200);
--
  PROCEDURE GenerateReferenceGrid(v_tablename IN VARCHAR2,
                                  v_xlo       IN NUMBER DEFAULT 0,
                                  v_ylo       IN NUMBER DEFAULT 0,
                                  v_xhi       IN NUMBER DEFAULT 100000,
                                  v_yhi       IN NUMBER DEFAULT 100000,
                                  v_divx      IN NUMBER DEFAULT 1000,
                                  v_divy      IN NUMBER DEFAULT 1000);
-- Data manipulation functions
  FUNCTION Geom2PtCluster(v_polygeom in SDO_GEOMETRY) RETURN SDO_GEOMETRY DETERMINISTIC;

-- Helper functions
  PROCEDURE FastFeatureClass(v_tablename IN VARCHAR2);
--
END GDODATA;
/
show errors
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
-- Package Body
CREATE OR REPLACE PACKAGE BODY GDOSYS.GDODATA IS
-- 
-- Adjustable Package Parameters
  v_defxlo     NUMBER  := 0;        -- Default lower left X
  v_defylo     NUMBER  := 0;        -- Default lower left Y
  v_defxhi     NUMBER  := 100000;   -- Default Upper Right X
  v_defyhi     NUMBER  := 100000;   -- Default Upper Right Y
--
  v_defElev    NUMBER  := 0;        -- Default Z or elevation value
--
-- Version and Date of Build
  c_pkg_version  CONSTANT VARCHAR2(12):='2016.002';      -- Current package version
  c_pkg_date     CONSTANT VARCHAR2(12):='07/06/2016';    -- Current package date
--
-- Globals
  v_mem         NUM_ARRAY;                      -- Internal Array
  i_counter     BINARY_INTEGER;                 -- Result counter
  i_other       BINARY_INTEGER;                 -- Temporary integer
  v_saved_norm  NUMBER;                         -- Random Normal distribution value
--
-- Global Storage Array Types Declarations
  TYPE T_INT    IS TABLE OF INTEGER;
  TYPE T_NUMBER IS TABLE OF NUMBER;
  TYPE T_DATE   IS TABLE OF DATE;
  TYPE T_CHAR   IS TABLE OF VARCHAR2(2048);
  TYPE T_GEOM   IS TABLE OF MDSYS.SDO_GEOMETRY;
--
-- Seed Lists
  -- Seed Lists
  i_listlen   PLS_INTEGER := 10;
  v_flist     T_CHAR  := T_CHAR('Chuck','Tina','Chris','Matt','Gary','Greg','Carolyn','Jennie','Bruce','Lisa');
  v_llist     T_CHAR  := T_CHAR('Douglas','Winston','Mirelli','Petersen','Robertson','Leonov','Woodbury','Young','Evans','Novak');
  v_clist     T_CHAR  := T_CHAR('Huntsville','Decatur','Hyderabad','London','Stockholm','Mumbai','Tokyo','Denver','Hong Kong','Prague');
  v_cntry     T_CHAR  := T_CHAR('USA','INDIA','CANADA','CZECH','CHINA','ENGLAND','GERMANY','SWEDEN','POLAND','RUSSIA');
-- -----------------------------------------------------------------------------
  PROCEDURE VERSION IS
    c_support     CONSTANT VARCHAR2(14) := 'Via Email Only';
    c_emailpath   CONSTANT VARCHAR2(29) := 'chuck.woodbury@intergraph.com';
    c_onlinehelp  CONSTANT VARCHAR2(20) := 'EXEC GDODATA.HELPME';
    i_width       PLS_INTEGER           :=66;
  BEGIN
    GOOM.DblLine( i_width);
    GOOM.TitleLine('INTERGRAPH/Hexagon Geospatial', i_width,'**');
    GOOM.TitleLine('GDODATA Package', i_width,'**');
    GOOM.TitleLine('Oracle Spatial Data Generator ', i_width,'**');
    GOOM.DblLine( i_width);
    GOOM.TitleLine('Author: Chuck Woodbury, Senior Technical Consultant', i_width,'**');
    GOOM.TitleLine('Hexagon Geospatial Division', i_width,'**');
    GOOM.TitleLine('Intergraph Technology Services', i_width,'**');
    GOOM.DblLine( i_width);
    GOOM.Response('Version', c_pkg_version, i_width,TRUE);
    GOOM.Response('Date', c_pkg_date, i_width,TRUE);
    GOOM.Response('Bug Reports and Support', c_support, i_width,TRUE);
    GOOM.Response('Email', c_emailpath, i_width,TRUE);
    GOOM.Response('Online Help', c_onlinehelp, i_width,TRUE);
    GOOM.DblLine( i_width);
  END VERSION;
-- -----------------------------------------------------------------------------
-- EXEC INIT(v_seed);  -- Initialize the seed value for the generation of random values
  PROCEDURE INIT (v_seed IN VARCHAR2) IS
    c_cmdname   VARCHAR2(32):='INIT';
    v_junk      VARCHAR2(2048);
    v_piece     VARCHAR2(20);
    v_randval   NUMBER;
    v_debug     VARCHAR2(255):='NONE';
    J           BINARY_INTEGER;
  BEGIN
    v_saved_norm := NULL;
    i_counter    := 0;
    i_other      := 24;
    v_junk       := TO_SINGLE_BYTE(v_seed);
    FOR I IN 0..54 LOOP
      v_piece    := SUBSTR(v_junk,1,19);
      v_randval  := 0;
      J          := 1;
      FOR J IN 1..19 LOOP
        v_randval  := 1E2*v_randval  + NVL(ASCII(SUBSTR(v_piece,J,1)),0.0);
      END LOOP;
      v_randval  := v_randval *1E-38 + I*.01020304050607080910111213141516171819;
      v_mem(I)   := MOD( v_randval , 1.0 );
      v_junk     := SUBSTR(v_junk,20);
    END LOOP;
    FOR J IN 0..10 LOOP
      FOR I IN 0..54 LOOP
        v_randval  := v_mem(MOD(I+55-1, 55)) * 1E24;
        v_randval  := MOD( v_randval , 1.0) + TRUNC(v_randval )*1E-38;
        v_randval  := v_mem(I)+v_randval ;
	   IF (v_randval  >= 1.0) THEN
          v_randval  := v_randval  - 1.0;
        END IF;
	 v_mem(I) := v_randval ;
      END LOOP;
    END LOOP;
  EXCEPTION
    WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_randval,v_debug,sqlcode,sqlerrm);
  END INIT;
-- ------------------------------------------------------------------------------------
-- Show the special character ascci codes.
  PROCEDURE ShowASCII IS
    I PLS_INTEGER;
    J PLS_INTEGER;
    K PLS_INTEGER;
  BEGIN
    FOR I IN 2..15 LOOP
      FOR J IN 1..16 LOOP
        K:=I*16+J;
        DBMS_OUTPUT.PUT((TO_CHAR(K,'000'))||':'||CHR(K)||'  ');
        IF K MOD 8 = 0 THEN
          DBMS_OUTPUT.PUT_LINE('');
        END IF;
      END LOOP;
    END LOOP;
  END ShowASCII;
-- ------------------------------------------------------------------------------------
--  FUNCTION VAL RETURN NUMBER;   result:=GDODATA.val;
--             Get a random Oracle number x, 0.0 <= x < 1.0
  FUNCTION VAL RETURN NUMBER IS
    c_cmdname   VARCHAR2(32):='VAL';
    v_randval   NUMBER;
    v_debug     VARCHAR2(255):='NONE';
  BEGIN
    i_counter := i_counter + 1;
    IF i_counter >= 55 THEN
      i_counter := 0;
      FOR I IN 0..30 LOOP
        v_randval  := v_mem(I+24) + v_mem(I);
	   IF (v_randval  >= 1.0) THEN
          v_randval  := v_randval  - 1.0;
        END IF;
        v_mem(I) := v_randval ;
      END LOOP;
      FOR I IN 31..54 LOOP
        v_randval  := v_mem(I-31) + v_mem(I);
	   IF (v_randval  >= 1.0) THEN
          v_randval  := v_randval  - 1.0;
        END IF;
        v_mem(I) := v_randval ;
      END LOOP;
    END IF;
    RETURN v_mem(i_counter);
  EXCEPTION
    WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_randval,v_debug,sqlcode,sqlerrm);
  END VAL;
-- ------------------------------------------------------------------------------------
--  FUNCTION VAL (LOW IN NUMBER, HIGH IN NUMBER) RETURN NUMBER;
--             result:=GDODATA.val(-180,180);
--             Get a random Oracle number x, LOW <= x < HIGH
  FUNCTION VAL ( v_LOW IN NUMBER, v_HIGH IN NUMBER) RETURN NUMBER IS
    c_cmdname  VARCHAR2(32):='VAL(LO,HI)';
  BEGIN
    RETURN (VAL*(v_HIGH-v_LOW))+v_LOW;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,'LOW:'||v_LOW,'HIGH:'||v_HIGH,sqlcode,sqlerrm);
  END VAL;
-- ------------------------------------------------------------------------------------
--  FUNCTION NORM RETURN NUMBER;
--             result:=GDODATA.NORM;
--             get a random number from a normal distribution
  FUNCTION NORM RETURN NUMBER IS  -- 38 DECIMAL PLACES: MEAN 0, VARIANCE 1
    c_cmdname  VARCHAR2(32):='NORM';
    v_V1  NUMBER;
    v_V2  NUMBER;
    v_R2  NUMBER;
    v_FAC NUMBER;
  BEGIN
    IF v_saved_norm IS NOT NULL THEN  -- SAVED FROM LAST TIME
      v_V1 := v_saved_norm;             -- TO BE RETURNED THIS TIME
      v_saved_norm := NULL;
    ELSE
      v_R2 := 2;
      WHILE v_R2 > 1 OR v_R2 = 0 LOOP   -- FIND TWO INDEPENDENT UNIFORM VARIABLES
        v_V1 := 2*VAL-1;
        v_V2 := 2*VAL-1;
        v_R2 := v_V1*v_V1 + v_V2*v_V2;        -- R2 IS RADIUS, THAT IS, DISTANCE FROM 0
      END LOOP;                     -- 0 < R2 <= 1:  IN UNIT CIRCLE
      -- NOW DERIVE TWO INDEPENDENT NORMALLY-DISTRIBUTED VARIABLES
      v_FAC := SQRT(-2*LN(v_R2)/v_R2);
      v_V1  := v_V1*v_FAC;         -- TO BE RETURNED THIS TIME
      v_saved_norm := v_V2*v_FAC; -- TO BE SAVED FOR NEXT TIME
    END IF;
    RETURN v_V1;
   EXCEPTION
     WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,'FAC:'||v_FAC,'NORM:'||v_saved_norm,sqlcode,sqlerrm);
  END  NORM;
-- ------------------------------------------------------------------------------------
--  FUNCTION STRING (OPT CHAR, LEN NUMBER)
--             STRING OF <LEN> CHARACTERS (MAX 255)
--             <OPT> specifies that the returned string may contain:
--             'u','U'  :  upper case alpha characters only
--             'l','L'  :  lower case alpha characters only
--             'a','A'  :  alpha characters only (mixed case)
--             'x','X'  :  any alpha-numeric characters (upper)
--             'p','P'  :  any printable characters
--
  FUNCTION STRING (v_OPT CHAR DEFAULT'X', v_LEN NUMBER DEFAULT 32)
    RETURN VARCHAR2 IS	-- STRING OF <LEN> CHARACTERS
    c_cmdname  VARCHAR2(32):='STRING';
    v_OPTX CHAR (1) := UPPER(v_OPT);
    v_LO   NUMBER;
    v_RNG  NUMBER;
    v_NUM  NUMBER;
    v_XSTR VARCHAR2 (255) := NULL;
  BEGIN
    IF    v_OPTX = 'U' THEN	-- UPPER CASE ALPHA CHARACTERS ONLY
      v_LO := 65; v_RNG := 26;	-- ASCII 41 TO 5A (HEX)
    ELSIF v_OPTX = 'L' THEN	-- LOWER CASE ALPHA CHARACTERS ONLY
      v_LO := 97; v_RNG := 26;	-- ASCII 61 TO 7A (HEX)
    ELSIF v_OPTX = 'A' THEN	-- ALPHA CHARACTERS ONLY (MIXED CASE)
      v_LO := 65; v_RNG := 52;	-- ASCII 41 TO 5A AND 61 TO 7A (SEE BELOW)
    ELSIF v_OPTX = 'X' THEN	-- ANY ALPHA-NUMERIC CHARACTERS (UPPER)
      v_LO := 48; v_RNG := 36;	-- ASCII 30 TO 39 AND 41 TO 5A (SEE BELOW)
    ELSIF v_OPTX = 'P' THEN	-- ANY PRINTABLE CHARACTERS
      v_LO := 32; v_RNG := 95;	-- ASCII 20 TO 7E (HEX)
    ELSE
      v_LO := 65; v_RNG := 26;    -- DEFAULT TO UPPER CASE
    END IF;
    FOR I IN 1 .. LEAST(v_LEN,255)
    LOOP
        -- GET RANDOM ASCII CHARACTER VALUE IN SPECIFIED RANGE
        v_NUM := v_LO + TRUNC(v_RNG * VAL); -- BETWEEN LO AND (LO + RNG -1)
        -- ADJUST FOR SPLIT RANGE */
      IF v_OPTX = 'A' AND v_NUM > 90 THEN
        v_NUM := v_NUM+6;	-- EXCLUDE ASCII CHARACTERS 5B TO 60
      ELSIF v_OPTX = 'X' AND v_NUM > 57 THEN
        v_NUM := v_NUM+7;	-- EXCLUDE ASCII CHARACTERS 3A TO 40
      END IF;
      v_XSTR := v_XSTR||CHR(v_NUM);	-- APPEND CHARACTER TO STRING
    END LOOP;
    RETURN v_XSTR;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,'Num:'||v_NUM,'String:'||v_XSTR,sqlcode,sqlerrm);
  END STRING;
-- -----------------------------------------------------------------------------------
-- FUNCTION RDATE (v_days IN NUMBER) RETURN DATE
-- Return a random date between now and a specified number of days forward + or back -
  FUNCTION RDATE (v_days IN NUMBER )RETURN DATE IS
  c_cmdname VARCHAR2(32):='RDATE';
  BEGIN
      RETURN (SYSDATE + FLOOR(VAL(0,v_days))) ;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,v_days,NULL,sqlcode,sqlerrm);
  END RDATE;
-- -----------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------
-- Math Calculation Functions
--
-- PI
  FUNCTION PI RETURN NUMBER IS
  BEGIN
    RETURN 3.1415926535897932384626433832795;
  END PI;
  -- Length of line
  FUNCTION LineLength ( v_X1 IN NUMBER, v_Y1 IN NUMBER, v_X2 IN NUMBER, v_Y2 IN NUMBER) RETURN NUMBER IS
  BEGIN
    RETURN SQRT(POWER( v_X2 - v_X1, 2) + POWER( v_Y2 - v_Y1, 2));
  END LineLength;
  -- Get radius of arc from 3 points
  FUNCTION GetArcRadius ( v_X1 in NUMBER, v_Y1 in NUMBER,
                          v_X2 in NUMBER, v_Y2 in NUMBER,
                          v_X3 in NUMBER, v_Y3 in NUMBER) RETURN NUMBER IS
  v_edge1     NUMBER;
  v_edge2     NUMBER;
  v_edge3     NUMBER;
  v_perim     NUMBER;
  v_Kfac      NUMBER;
  BEGIN
    -- Caluclate the length of each side of the triangle.
    v_edge1 := LineLength( v_X1, v_Y1, v_X2, v_Y2);
    v_edge2 := LineLength( v_X2, v_Y2, v_X3, v_Y3);
    v_edge3 := LineLength( v_X3, v_Y3, v_X1, v_Y1);
    -- Calulate perimeter divided by 2
    v_perim := 0.5 * ( v_edge1 + v_edge2 + v_edge3);
    -- Calulate the K factor.
    v_Kfac := SQRT( v_perim*( v_perim - v_edge1)*( v_perim - v_edge2)*( v_perim - v_edge3));
    -- Us K factor to calculate Radius.
    RETURN ( v_edge1 * v_edge2 * v_edge3)/(4 * v_Kfac);
  EXCEPTION
    WHEN OTHERS THEN
       RETURN 0;
  END GetArcRadius;
  -- Factorial of a number (n!) - a rucursive function.
  FUNCTION fac (v_num POSITIVE) RETURN INTEGER IS
  BEGIN
    IF v_num = 1 THEN  -- terminating condition
       RETURN 1;
    ELSE
       RETURN v_num * fac( v_num - 1 );  -- recursive call
    END IF;
  END fac;
  -- GreatestCommonDivisor of two positive integers
  FUNCTION GCD ( v_intA IN INTEGER, v_intB IN INTEGER ) RETURN INTEGER IS
    a INTEGER;
    b INTEGER;
    c INTEGER;
  BEGIN
    a:= v_intA;
    b:= v_intB;
    IF a <= b THEN
      c:= a;
      a:= b;
      b:= c;
    END IF;
    LOOP
      c := MOD(a, b);
      IF (c = 0) THEN
        EXIT;
      END IF;
      a := b;
      b := c;
    END LOOP;
    RETURN b;
  END GCD;
  -- Percent Difference
  FUNCTION DIFF (X1 IN NUMBER DEFAULT 1, X2 IN NUMBER DEFAULT 1) RETURN NUMBER IS
  BEGIN
    RETURN 100 * ABS((X1-X2)/((X1+X2)/2));
  END;
-- -----------------------------------------------------------------------------------
-- Convert a geometry to a cluster of points..
  FUNCTION Geom2PtCluster( v_polygeom in SDO_GEOMETRY) RETURN SDO_GEOMETRY DETERMINISTIC IS
  c_cmdname       VARCHAR2(30):='Geom2PtCluster';
  -- Initialize working geometry
  v_polygeometry  SDO_GEOMETRY;
  -- Initialize output pt cluster
  v_ptgeom        SDO_GEOMETRY:=MDSYS.SDO_GEOMETRY(NULL,NULL,NULL,SDO_ELEM_INFO_ARRAY(),SDO_ORDINATE_ARRAY());
  -- Other variables
  v_arctol        NUMBER:=0.1;    -- Arc Stroking must be min 20X geom tolerance.
  v_geomtol       NUMBER:=0.0005; -- Tolerance used in GeoMedia.
  v_gtype         INTEGER;        -- Geometry Type
  v_numpts        INTEGER;        -- Num of output pts
  v_numords       INTEGER;        -- Num of input ordinates
  v_dim           INTEGER;        -- Dimension of geometry
  BEGIN
    -- Process the input geometry
    -- First, get rid of any arcs so geometry is simple.
    v_polygeometry:= SDO_GEOM.SDO_ARC_DENSIFY(v_polygeom,v_geomtol,'arc_tolerance='|| v_arctol ||''''); 
    v_gtype       := v_polygeometry.SDO_GTYPE;                        -- Get the GTYPE
    v_dim         :=SUBSTR(TO_CHAR( v_gtype ),1,1);                      -- Get the dimensions            
    IF v_gtype IN (2003,2007,3003,3007) THEN
      -- Cpolygon to linestring.  This helps in hole processing.
      v_polygeometry:= SDO_UTIL.PolygonToLine( v_polygeometry );
      v_numpts      := ( v_polygeometry.sdo_ordinates.count / v_dim);
    ELSIF v_gtype IN (2002,2006,3002,3006) THEN
      -- Get the total number of points in line string
      v_numpts :=( v_polygeometry.sdo_ordinates.count / v_dim);      
    ELSE
      -- Wrong GTYPE, exit with input geometry
      RETURN v_polygeom; 
    END IF;                                  
    -- Create the output point cluster geometry.
    IF v_dim = 2 THEN
      v_ptgeom.SDO_GTYPE:=2005;                                  -- Two Dimensional Point Cluster             
    ELSE
      v_ptgeom.SDO_GTYPE:=3005;                                  -- Three Dimensional Point Cluster
    END IF;
    v_numords := v_polygeometry.sdo_ordinates.count;               -- Number of ordinates to process      
    v_ptgeom.SDO_ELEM_INFO.EXTEND(3);                              -- Initialize ELEM_INFO
    v_ptgeom.SDO_ELEM_INFO(1) := 1;                                -- First Ordinate
    v_ptgeom.SDO_ELEM_INFO(2) := 1;                                -- Etype for pt
    v_ptgeom.SDO_ELEM_INFO(3) := v_numpts;                         -- Num pts in cluster
    v_ptgeom.SDO_ORDINATES.EXTEND( v_numords );                    -- Initialize Ordinate Array by number of pts
    FOR i IN 1..v_numords LOOP                                     -- Loops thru ordinates
     v_ptgeom.SDO_ORDINATES(i) := v_polygeometry.SDO_ORDINATES(i); -- Assign ordinates to new pt geometry
    END LOOP;
    -- Remove duplicate points
    v_ptgeom:=SDO_UTIL.Remove_Duplicate_Vertices( v_ptgeom, v_geomtol);
    RETURN v_ptgeom;
  EXCEPTION
    WHEN OTHERS THEN
      -- An error has occured, handle and return input geometry.
      GOOM.REPORT_ERROR ( c_cmdname, v_gtype, v_numpts,sqlcode,sqlerrm); 
      RETURN v_polygeom;
  END Geom2PtCluster;
-- -----------------------------------------------------------------------------------
-- -----------------------------------------------------------------------------------
-- Random Geometry functions
--
-- RANDOM_PT returns a random point geometry in the specified range.
-- xlo,xhi,ylo,yhi is the range for the pt
-- geom:=RANDOM_PT(xlo,xhi,ylo,yhi,rotation_angle,pt_type);
-- geom:=RANDOM_PT;  To use defaults
  FUNCTION RANDOM_PT( v_xlo IN NUMBER DEFAULT 0, 
                      v_xhi IN NUMBER DEFAULT 100000, 
                      v_ylo IN NUMBER DEFAULT 0, 
                      v_yhi IN NUMBER DEFAULT 100000,
                      v_rot IN NUMBER DEFAULT NULL,
                      v_dim IN NUMBER DEFAULT 3,
                      v_typ IN VARCHAR2 DEFAULT 'O')
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC IS
  c_cmdname   VARCHAR2(32):='RANDOM_PT';
  v_temp_geom MDSYS.SDO_GEOMETRY:=MDSYS.SDO_GEOMETRY(NULL, NULL, NULL,MDSYS.SDO_ELEM_INFO_ARRAY(),MDSYS.SDO_ORDINATE_ARRAY());
  v_xcoord    NUMBER;
  v_ycoord    NUMBER;
  v_zcoord    NUMBER;
  v_rotation  NUMBER;
  v_debug     VARCHAR2(8):='NONE';
  BEGIN
        v_xcoord := VAL( v_xlo, v_xhi) ;
        v_ycoord := VAL( v_ylo, v_yhi) ;
        v_zcoord := VAL( 0,1000) ;
        IF v_rot is NULL THEN
          v_rotation := VAL(0,360);
        ELSE
          v_rotation := v_rot;
        END IF;
        CASE v_typ
         WHEN 'N' THEN
          IF v_dim=2 THEN
            v_temp_geom:=MDSYS.SDO_GEOMETRY(2001, NULL,MDSYS.SDO_POINT_TYPE(0,0,NULL),NULL,NULL);
            v_temp_geom.sdo_point.X:= v_xcoord;
            v_temp_geom.sdo_point.Y:= v_ycoord;
          ELSE
            v_temp_geom:=MDSYS.SDO_GEOMETRY(3001, NULL,MDSYS.SDO_POINT_TYPE(0,0,0),NULL,NULL);
            v_temp_geom.sdo_point.X:= v_xcoord;
            v_temp_geom.sdo_point.Y:= v_ycoord;
            v_temp_geom.sdo_point.Z:= v_zcoord;
          END IF;
         ELSE
          IF v_dim=2 THEN
            v_temp_geom:=MDSYS.SDO_GEOMETRY(2001, NULL, NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1,1,3,1,0),MDSYS.SDO_ORDINATE_ARRAY());
            v_temp_geom.sdo_ordinates.extend (2);
            v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count-1):= v_xcoord;
            v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count):= v_ycoord;
            v_temp_geom.sdo_ordinates.extend (2);
            v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count-1):=GOOM.ROTINDEX('I',v_rotation);
            v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count):=GOOM.ROTINDEX('J',v_rotation);
          ELSE
            v_temp_geom:=MDSYS.SDO_GEOMETRY(3001, NULL, NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1,1,4,1,0),MDSYS.SDO_ORDINATE_ARRAY());
            v_temp_geom.sdo_ordinates.extend (3);
            v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count-2):= v_xcoord;
            v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count-1):= v_ycoord;
            v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count):= v_zcoord;
            v_temp_geom.sdo_ordinates.extend (3);
            v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count-2):=GOOM.ROTINDEX('I',v_rotation);
            v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count-1):=GOOM.ROTINDEX('J',v_rotation);
            v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count):=0;
          END IF;
         END CASE;
        RETURN v_temp_geom;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,'X:'||v_xcoord||','||'Y:'||v_ycoord,v_debug,sqlcode,sqlerrm);
  END RANDOM_PT;
--
-- RANDOM_LINE returns a random line geometry in the specified range.
-- geom:=RANDOM_LINE(xlo,xhi,ylo,yhi,varlo, varhi, maxver);
-- xlo,xhi,ylo,yhi is the range for the line
-- varlo,varhi is the min and max variation for the distance to the next vertice.
-- maxver is the maximum number of vertices allied in the line.
-- geom:=RANDOM_LINE;  to use defaults
  FUNCTION RANDOM_LINE( v_xlo IN NUMBER DEFAULT 0, 
                        v_xhi IN NUMBER DEFAULT 100000, 
                        v_ylo IN NUMBER DEFAULT 0, 
                        v_yhi IN NUMBER DEFAULT 100000, 
                      v_varlo IN NUMBER DEFAULT 250, 
                      v_varhi IN NUMBER DEFAULT 250,
                     v_maxver IN NUMBER DEFAULT 25,
                        v_dim IN NUMBER DEFAULT 3) 
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC IS
  --
  c_cmdname   VARCHAR2(32):='RANDOM_LINE';
  v_temp_geom MDSYS.SDO_GEOMETRY;
  v_zlo       NUMBER:=0;
  v_zhi       NUMBER:=1000;
  v_xcoord    NUMBER(32,6):=0;
  v_ycoord    NUMBER(32,6):=0;
  v_zcoord    NUMBER(32,6):=0;
  v_xorigin   NUMBER(32,6):=0;
  v_yorigin   NUMBER(32,6):=0;
  v_vertices  NUMBER:=2;
  v_variation NUMBER(32,6):=0;
  v_debug     VARCHAR2(255):='NONE';
  --
  BEGIN
     v_vertices:=GOOM.RandInRange(2,v_maxver);
     IF v_dim=2 THEN
       v_temp_geom:=MDSYS.SDO_GEOMETRY(2002, NULL, NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),MDSYS.SDO_ORDINATE_ARRAY());
     ELSE
       v_temp_geom:=MDSYS.SDO_GEOMETRY(3002, NULL, NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1),MDSYS.SDO_ORDINATE_ARRAY());
     END IF; 
     FOR v_count IN 1..v_vertices LOOP
        v_variation:=VAL(v_varlo,v_varhi);
        IF v_count = 1 THEN
           v_debug:='Initial Vertice';
           v_xcoord:=VAL(v_xlo,v_xhi);
           v_ycoord:=VAL(v_ylo,v_yhi);
           --v_zcoord:=VAL(v_zlo,v_zhi);
           v_zcoord:=0;
           v_xorigin:=v_xcoord;
           v_yorigin:=v_ycoord;
         ELSE
           v_debug:='Subsequent Vertices';
           v_xcoord:=VAL(v_xcoord-v_variation,v_xcoord+v_variation);
           v_ycoord:=VAL(v_ycoord-v_variation,v_ycoord+v_variation);
         END IF;
         v_debug:='Assign Ordinate Array';
         IF v_dim=2 THEN
           v_temp_geom.sdo_ordinates.extend (2);
           v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count-1):= v_xcoord;
           v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count)  := v_ycoord;
         ELSE
           v_temp_geom.sdo_ordinates.extend (3);
           v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count-2):= v_xcoord;
           v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count-1):= v_ycoord;
           v_temp_geom.sdo_ordinates(v_temp_geom.sdo_ordinates.count)  := v_zcoord;
         END IF;
      END LOOP;
      RETURN v_temp_geom;
    EXCEPTION
      WHEN OTHERS THEN
        GOOM.REPORT_ERROR (c_cmdname,'X:'||v_xcoord||','||'Y:'||v_ycoord,v_debug,sqlcode,sqlerrm);
  END RANDOM_LINE;
-- 
-- RANDOM_POLY returns a random polygon geometry in the specified range.
-- geom:=RANDOM_POLY(scale,dimension,xlo,xhi,ylo,yhi);
-- xlo,xhi,ylo,yhi is the range for the poly
-- scale regulates the maximum size of the polygon.
-- geom:=RANDOM_POLY;  to use defaults
  FUNCTION RANDOM_POLY(v_scale IN INTEGER DEFAULT 10,
                         v_dim IN INTEGER DEFAULT 3,  
                         v_Xlo IN NUMBER  DEFAULT 0, 
                         v_Xhi IN NUMBER  DEFAULT 100000,
                         v_ylo IN NUMBER  DEFAULT 0,
                         v_yhi IN NUMBER  DEFAULT 100000,
                         v_typ IN INTEGER DEFAULT 1) 
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC AS
  --
  c_cmdname    VARCHAR2(32):='Random_Poly';
  v_temp_geom  MDSYS.SDO_GEOMETRY:= MDSYS.SDO_GEOMETRY (2005, NULL, NULL,MDSYS.SDO_ELEM_INFO_ARRAY(),MDSYS.SDO_ORDINATE_ARRAY ());
  v_area       MDSYS.SDO_GEOMETRY;
  v_maxpts     INTEGER:=10;
  v_minsize    INTEGER:=10;
  v_maxsize    INTEGER:=50;
  --
  v_numpts     INTEGER;
  v_xboxsize   INTEGER;
  v_yboxsize   INTEGER;
  v_box_xhi    NUMBER;
  v_box_yhi    NUMBER;
  v_xorigin    NUMBER;
  v_yorigin    NUMBER;
  v_pt_ord     INTEGER;
  v_zvalue     NUMBER;
  v_debug      VARCHAR2(255);
  --
  begin
  -- Build a bounding box for the random polygon.
     v_maxpts:=v_scale;                         -- Tie max number of points in cluster to scale.
     v_numpts:=GDODATA.VAL(3,v_maxpts);         -- Random number of points in cluster; 3 min,
     v_xorigin:=GDODATA.VAL(v_xlo,v_xhi);       -- Calculate X origin of box
     v_yorigin:=GDODATA.VAL(v_ylo,v_yhi);       -- Calculate Y origin of box
     v_xboxsize:=v_scale*GDODATA.VAL(v_minsize,v_maxsize);  -- Calculate random box size for X 10 to 50
     v_yboxsize:=v_scale*GDODATA.VAL(v_minsize,v_maxsize);  -- Calculate random box size in Y 10 to 50
     v_box_xhi:=v_xorigin+(GDODATA.VAL(10,v_xboxsize));     -- Calculate random Box X End point
     v_box_yhi:=v_yorigin+(GDODATA.VAL(10,v_yboxsize));     -- Calculate random Box Y End point
  -- Build the geometry for a cluster of random points within the box
     v_temp_geom.sdo_elem_info.extend (3);          -- Extend array to elem info
     v_temp_geom.sdo_elem_info(1):=1;               -- First Ordinate
     v_temp_geom.sdo_elem_info(2):=1;               -- Point Cluster
     v_temp_geom.sdo_elem_info(3):=v_numpts;        -- Number of points in cluster
     v_temp_geom.sdo_ordinates.extend(v_numpts*2);  -- Extend ordinate array to hold points
     v_debug:='Variable Assignments Complete.';
     FOR I IN 1..v_numpts LOOP                      -- Create point cluster geometry.
       v_pt_ord:=I*2;
       v_temp_geom.sdo_ordinates(v_pt_ord-1):=GDODATA.VAL(v_xorigin,v_box_xhi);  -- Random point in box X ord
       v_temp_geom.sdo_ordinates(v_pt_ord):=GDODATA.VAL(v_yorigin,v_box_yhi);    -- Random point in box Y ord
     END LOOP;
     -- Build the random polygon ...
     IF v_typ = 1 THEN
       v_area:=SDO_GEOM.SDO_CONVEXHULL(v_temp_geom,0.0005);  -- ...based on the convex hull of input points
--     ELSE
--       v_area:=SDO_GEOM.SDO_CONCAVEHULL(v_temp_geom,0.0005); -- ...based on the concave hull of input points
     END IF;
     IF v_dim = 3 THEN
       RETURN GOOM.Convert3D(v_area, 0);                     -- Add z if requested. then return.
     ELSE
       RETURN v_area;                                        -- return the poly to the calling procedure.
     END IF;
   EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,'Error',v_debug,sqlcode,sqlerrm);
  END Random_Poly;
--
-- LORENZ returns a geometry containing a LORENZ linestring.
-- geom:=LORENZ(scale,iteration,Xorigin,Yorigin);
  FUNCTION LORENZ( v_scale IN NUMBER DEFAULT 10,
               v_iteration IN NUMBER DEFAULT 800,
                 v_xorigin IN NUMBER DEFAULT 0,
                 v_yorigin IN NUMBER DEFAULT 0 )
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC AS
    --
    c1      NUMBER:= 0.292893;
    dt      NUMBER:= 0.02;
    a       NUMBER:= 5;
    b       NUMBER:= 15;
    c       NUMBER:= 1;
    --
    xa      DBMS_SQL.NUMBER_TABLE;
    ya      DBMS_SQL.NUMBER_TABLE;
    za      DBMS_SQL.NUMBER_TABLE;
    --
    x1      NUMBER;
    y1      NUMBER;
    z1      NUMBER;
    x       NUMBER;
    y       NUMBER;
    z       NUMBER;
    --
    xd      NUMBER;
    yd      NUMBER;
    --
    v_geom  MDSYS.SDO_GEOMETRY:=SDO_GEOMETRY(2002,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1), MDSYS.SDO_ORDINATE_ARRAY());
  BEGIN
    xa(1) :=3.051522;
    ya(1) :=1.592542;
    za(1) :=15.62388;
    xa(2) :=xa(1);
    ya(2) :=2.582542;
    za(2) :=za(1);
    FOR i IN 1..v_iteration LOOP
        FOR j IN 1..2 LOOP
      	 x := xa(j);
      	 y := ya(j);
      	 z := za(j);
      	x1 := x - a*x*dt + a*y*dt;
      	y1 := y + b*x*dt - y*dt - z*x*dt;
      	z1 := z - c*z*dt + x*y*dt;
      	 x := x1;
      	 y := y1;
      	 z := z1;
      	xd := v_xorigin + (y - x*c1)*v_scale;
      	yd := v_yorigin + (z + x*c1)*v_scale;
          v_geom.SDO_ORDINATES.EXTEND(2);
          v_geom.sdo_ordinates(v_geom.sdo_ordinates.count-1):= xd;
          v_geom.sdo_ordinates(v_geom.sdo_ordinates.count)  := yd;
      	xa(j) := x;
      	ya(j) := y;
      	za(j) := z;
        END LOOP;
    END LOOP;
    RETURN v_geom;
  END LORENZ;
-- 
-- SERPENSKI returns a geometry containing a SERPENSKI linestring.
-- geom:=LORENZ(scale,iteration,Xorigin,Yorigin);
  FUNCTION SERPENSKI( v_scale IN NUMBER DEFAULT 1,
                  v_iteration IN NUMBER DEFAULT 400,
                    v_xorigin IN NUMBER DEFAULT 0,
                    v_yorigin IN NUMBER DEFAULT 0 )
            RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC AS
    --
    x1        INTEGER := 320;
    y1        INTEGER :=   0;
    x2        INTEGER := 320;
    y2        INTEGER :=   0;
    --
    v_direct    INTEGER;
    v_geom      MDSYS.SDO_GEOMETRY:=SDO_GEOMETRY(2002,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1), MDSYS.SDO_ORDINATE_ARRAY()); 
  BEGIN
    FOR i IN 1 .. v_iteration LOOP
      v_direct:=GDODATA.val(0,3);
      IF v_direct = 0 THEN
        x1 := (x2 + 320) / 2;
        y1 := (y2 + 0) / 2;
      ELSIF v_direct = 1 THEN
       x1 := (x2 + 0) / 2;
       y1 := (y2 + 480) / 2;
      ELSIF v_direct = 2 THEN
    	x1 := v_xorigin + ((x2 + 640) / 2) * v_scale;	
    	y1 := v_yorigin + ((y2 + 480) / 2) * v_scale;
      ELSE
        NULL;
      END IF;
      v_geom.SDO_ORDINATES.EXTEND(2);
      v_geom.sdo_ordinates(v_geom.sdo_ordinates.count-1):= x1;
      v_geom.sdo_ordinates(v_geom.sdo_ordinates.count)  := y1;
      x2 := x1;
      y2 := y1;
    END LOOP;
    RETURN v_geom;
  END SERPENSKI;
--
-- SPIROGRAPH returns a geometry containing a SPIROGRAPH design.
-- geom:=SPIROGRAPH(scale,resolution,cycles,LargeRadius,SmallRadius,PenParam,Xorigin,Yorigin);
  FUNCTION SPIROGRAPH( v_scale   IN NUMBER         DEFAULT 10,
                       v_res     IN NUMBER         DEFAULT 10,
                       v_cycles  IN NUMBER         DEFAULT 2,
                       v_Aradius IN NUMBER         DEFAULT 100,
                       v_Bradius IN NUMBER         DEFAULT 20,
                       v_param   IN NUMBER         DEFAULT 80,
                       v_xorigin IN NUMBER         DEFAULT 0,
                       v_yorigin IN NUMBER         DEFAULT 0)
           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC AS
    --
    v_Xval     NUMBER;
    v_Yval     NUMBER;
    v_radians  NUMBER;
    --
    v_geom     MDSYS.SDO_GEOMETRY:=SDO_GEOMETRY(2002,NULL,NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,2,1), MDSYS.SDO_ORDINATE_ARRAY());
    --
  BEGIN
    FOR I IN 0..360*v_res*v_cycles LOOP
      v_radians:=GOOM.ANG2RAD(I/v_res);
      v_Xval := v_xorigin+((v_Aradius-v_Bradius)*cos(v_radians) + v_param*cos((v_Aradius-v_Bradius)*v_radians/v_Bradius)*v_scale);
      v_Yval := v_yorigin+((v_Aradius-v_Bradius)*sin(v_radians) - v_param*sin((v_Aradius-v_Bradius)*v_radians/v_Bradius)*v_scale);
      --
      v_geom.SDO_ORDINATES.EXTEND(2);
      v_geom.sdo_ordinates(v_geom.sdo_ordinates.count-1) := v_Xval;
      v_geom.sdo_ordinates(v_geom.sdo_ordinates.count)   := v_Yval;
    END LOOP;
    RETURN v_geom;
END SPIROGRAPH;
-- ----------------------------------------------------------------------------------------
-- Create_Poly generates a poly using a standard shape at a specified size and location.
--
  FUNCTION CREATE_POLY(v_poly_type IN INTEGER, v_scale IN INTEGER, v_Xorigin in NUMBER DEFAULT NULL, v_Yorigin in NUMBER DEFAULT NULL) RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC AS
    c_cmdname    VARCHAR2(32):='Create_Poly';
    v_temp_geom  MDSYS.SDO_GEOMETRY:= MDSYS.SDO_GEOMETRY (2003, NULL, NULL,
                                                          MDSYS.SDO_ELEM_INFO_ARRAY (),
                                                          MDSYS.SDO_ORDINATE_ARRAY ());
    v_ords          INTEGER;
    v_pairs         INTEGER;
    v_ordinate      INTEGER;
    v_xo            NUMBER;
    v_yo            NUMBER;
  begin
    IF v_poly_type =1 THEN --simple shape with one arc
    v_ords:=12;
    v_temp_geom:=MDSYS.SDO_GEOMETRY(2003, NULL, NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1005,3,1,2,1,5,2,2,9,2,1),MDSYS.SDO_ORDINATE_ARRAY(0,0,4,0,4,4,2,6,0,4,0,0));
    ELSIF v_poly_type =2 THEN -- complex polygon with 2 arcs
    v_ords:=26;
    v_temp_geom:=MDSYS.SDO_GEOMETRY(2003, NULL, NULL,
                 MDSYS.SDO_ELEM_INFO_ARRAY(1,1005,5,1,2,1,5,2,2,9,2,1,17,2,2,21,2,1),
                 MDSYS.SDO_ORDINATE_ARRAY(0,0,4,0,8,0,11,1,12,4,12,8,12,12,8,12,8,8,7,5,4,4,0,4,0,0));
    ELSE -- complex with holes
    v_ords:=38;
    v_temp_geom:=MDSYS.SDO_GEOMETRY(2003, NULL, NULL,
                 MDSYS.SDO_ELEM_INFO_ARRAY(1,1005,4,1,2,2,5,2,1,9,2,2,13,2,1,19,2005,2,19,2,2,23,2,2,29,2003,1 ),
                 MDSYS.SDO_ORDINATE_ARRAY(0,0,2,-1,4,0,8,0,5,8,3,9,2,8,-3,8,0,0, 3,2,4,3,5,2,4,1,3,2, 0,5,0,7,3,7,3,5,0,5));
    END IF; 
    -- Adjust for origin and scale.
    v_pairs:=v_ords/2;
    FOR v_count in 1..v_pairs LOOP
      IF v_Xorigin IS NULL OR v_Yorigin IS NULL THEN
        v_xo:=VAL(0,100000);
        v_yo:=VAL(0,100000);
      ELSE
        v_xo:=v_Xorigin;
        v_yo:=v_Yorigin;
      END IF;
      v_ordinate:=v_count*2;
      v_temp_geom.sdo_ordinates(v_ordinate-1)   :=v_xo+v_scale*(v_temp_geom.sdo_ordinates(v_ordinate-1));
      v_temp_geom.sdo_ordinates(v_ordinate)     :=v_yo+v_scale*(v_temp_geom.sdo_ordinates(v_ordinate));
    END LOOP;
    RETURN v_temp_geom;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,'Error','Error',sqlcode,sqlerrm);
  END CREATE_POLY;
-- ----------------------------------------------------------------------------------------
-- Misc internal functions and procedures.
--
  FUNCTION CreateTrigName (v_tablename IN VARCHAR2) RETURN VARCHAR2 IS
  c_cmdname       VARCHAR2(30):='CreateTrigName';
  v_suffix        VARCHAR2(5):='_TRG';
  v_ownertable    VARCHAR2(61);
  v_owner         VARCHAR2(30);
  v_table         VARCHAR2(30);
  v_count         PLS_INTEGER:=1;
  v_trigname      VARCHAR2(128);
  v_debug         VARCHAR2(30);
  BEGIN
    v_ownertable   := GOOM.GetOwnerObject(v_tablename);
    v_owner        := GOOM.SplitOwnerObject(v_ownertable,'OWNER');
    v_table        := GOOM.SplitOwnerObject(v_ownertable,'TABLE');
    v_trigname:=SUBSTR(v_table,1,24)||v_suffix;
    WHILE GOOM.chkTrigger(v_owner||'.'||v_trigname) LOOP
      v_trigname:=SUBSTR(v_table,1,24)||v_count||v_suffix;
      v_count:=v_count+1;
    END LOOP;
    RETURN v_trigname;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,v_ownertable,v_owner,v_table,sqlerrm);
      RETURN NULL;
  END CreateTrigName;
-- ----------------------------------------------------------------------------------------
  PROCEDURE CreateTestTable (v_tablename IN VARCHAR2, 
                               v_tabtype IN VARCHAR2 DEFAULT 'L',
                               v_keytype IN VARCHAR2 DEFAULT 'I', 
                              v_geomtype IN VARCHAR2 DEFAULT NULL)  
  IS
  c_cmdname     VARCHAR2(32)  := 'CreateTestTable';
  v_seqname     VARCHAR2(61)  := NULL;
  v_trigname    VARCHAR2(32)  := NULL;
  v_sql         VARCHAR2(2048):= NULL;
  v_key         VARCHAR2(255) := NULL;
  v_comment     VARCHAR2(255) := NULL;
  v_trig        VARCHAR2(2048):= NULL;
  v_geomtype_in VARCHAR2(2)   := NULL;
  v_key_col     VARCHAR2(30)  := NULL;
  v_debug       VARCHAR2(32)  := 'INIT';
  BEGIN
  -- Create the primary key type
  v_trigname:=CreateTrigName(v_tablename);
  CASE UPPER(v_keytype)
    WHEN 'C' THEN
     v_key:='CID VARCHAR2(32) PRIMARY KEY';
     v_key_col:='CID';
     v_seqname:=GOOM.GetSequenceName(v_tablename,v_key_col);
     IF v_trigname IS NOT NULL THEN
     v_trig:='CREATE OR REPLACE TRIGGER '||v_trigname||' BEFORE INSERT ON '||v_tablename||
              ' REFERENCING NEW AS NEW FOR EACH ROW 
                DECLARE
                 v_sql VARCHAR2(255);
                BEGIN
                 IF :NEW.CID IS NULL THEN 
                   v_sql:=''SELECT ''''CID''''||'||v_seqname||'.nextval FROM dual''; 
                   EXECUTE IMMEDIATE v_sql INTO :NEW.CID;
                 END IF;
                END;';
     END IF;
    WHEN 'S' THEN
     v_key:='SYS_ID VARCHAR2(32) PRIMARY KEY';
     v_key_col:='SYS_ID';
     v_seqname:=GOOM.GetSequenceName(v_tablename,v_key_col);
     IF v_trigname IS NOT NULL THEN
     v_trig:='CREATE OR REPLACE TRIGGER '||v_trigname||' BEFORE INSERT ON '||v_tablename||
              ' REFERENCING NEW AS NEW FOR EACH ROW 
                DECLARE
                 v_sql VARCHAR2(255);
                BEGIN
                 IF :NEW.SYS_ID IS NULL THEN 
                   v_sql:=''SELECT SYS_GUID() FROM dual''; 
                   EXECUTE IMMEDIATE v_sql INTO :NEW.SYS_ID;
                 END IF;
                END;';
      END IF;
    WHEN 'R' THEN
     v_key:='N38ID NUMBER(38) PRIMARY KEY';
     v_key_col:='N38ID';
     v_seqname:=GOOM.GetSequenceName(v_tablename,v_key_col);
     IF v_trigname IS NOT NULL THEN
     v_trig:='CREATE OR REPLACE TRIGGER '||v_trigname||' BEFORE INSERT ON '||v_tablename||
              ' REFERENCING NEW AS NEW FOR EACH ROW 
                DECLARE
                 v_sql VARCHAR2(255);
                BEGIN
                 IF :NEW.N38ID IS NULL THEN 
                   v_sql:=''SELECT '||v_seqname||'.nextval FROM dual''; 
                   EXECUTE IMMEDIATE v_sql INTO :NEW.N38ID;
                 END IF;
                END;';
      END IF;
    ELSE
     v_key:='PID INTEGER PRIMARY KEY';
     v_key_col:='PID';
     v_seqname:=GOOM.GetSequenceName(v_tablename,v_key_col);
     IF v_trigname IS NOT NULL THEN
     v_trig:='CREATE OR REPLACE TRIGGER '||v_trigname||' BEFORE INSERT ON '||v_tablename||
              ' REFERENCING NEW AS NEW FOR EACH ROW 
                DECLARE
                 v_sql VARCHAR2(255);
                BEGIN
                 IF :NEW.PID IS NULL THEN 
                   v_sql:=''SELECT '||v_seqname||'.nextval FROM dual''; 
                   EXECUTE IMMEDIATE v_sql INTO :NEW.PID;
                 END IF;
                END;';
     END IF;
  END CASE;
  -- Create the table
  IF UPPER(v_tabtype) = 'L' THEN  -- Long Table
      v_sql:='CREATE TABLE '||v_tablename||'('||v_key||',
                         CREATION_DATE  DATE DEFAULT SYSDATE,
                         RANDOM_DATE    DATE,
                         COMMENTS       CLOB,
                         LONG_TEXT      VARCHAR2(255) DEFAULT ''LONG TEXT'',
                         SHORT_TEXT     VARCHAR2(32)  DEFAULT ''SHORT TEXT'',
                         NUMBER52       NUMBER(5,2)   DEFAULT 123.45,
                         NUMBER10       NUMBER(10,0)  DEFAULT 123456789,
                         NUMBER16       NUMBER(16,3)  DEFAULT 123456.789,
                         NUMBER38A      NUMBER(38)    DEFAULT 1234567890123456789,
                         NUMBER38B      NUMBER(38)    DEFAULT 1234567890123456789.1234567890123456789,
                         NUMBER_INTEGER INTEGER       DEFAULT 100,
                         BOOLEAN_FLAG   NUMBER(1,0)   DEFAULT 1,
                         GEOMETRY       MDSYS.SDO_GEOMETRY)';
      v_comment:='Long table generated with GDODATA package. Trigger and Sequence included.';
  ELSIF UPPER(v_tabtype) = 'A' THEN  -- Attrbute Table Only
      v_sql:='CREATE TABLE '||v_tablename||'('||v_key||',
                         CREATION_DATE  DATE DEFAULT SYSDATE,
                         RANDOM_DATE    DATE,
                         COMMENTS       CLOB,
                         LONG_TEXT      VARCHAR2(255) DEFAULT ''LONG TEXT'',
                         SHORT_TEXT     VARCHAR2(32)  DEFAULT ''SHORT TEXT'',
                         NUMBER52       NUMBER(5,2)   DEFAULT 123.45,
                         NUMBER10       NUMBER(10,0)  DEFAULT 123456789,
                         NUMBER16       NUMBER(16,3)  DEFAULT 123456.789,
                         NUMBER38A      NUMBER(38)    DEFAULT 1234567890123456789,
                         NUMBER38B      NUMBER(38)    DEFAULT 1234567890123456789.1234567890123456789,
                         NUMBER_INTEGER INTEGER       DEFAULT 100,
                         BOOLEAN_FLAG   NUMBER(1,0)   DEFAULT 1)';
      v_comment:='Attribute Data Only.';
  ELSE -- Simple Table
      v_sql:='CREATE TABLE '||v_tablename||'('||v_key||',
                         COMMENTS       VARCHAR2(255) DEFAULT ''Manually Entered'',
                         GEOMETRY       MDSYS.SDO_GEOMETRY)';
      v_comment:='Simple table generated with GDODATA package. Trigger and Sequence included.';
  END IF;
  v_debug:='Statements created';
  -- Check for existing table/seq and delete. Trigger in unaffected.
  IF GOOM.chkTable(UPPER(v_tablename)) THEN
     GOOM.DropTable(UPPER(v_tablename));
     v_debug:='Processed drop';
  END IF;
  -- Create the test table, sequence, and trigger
  EXECUTE IMMEDIATE v_sql;
  v_debug   :='Processed table';
  GOOM.CreateSequence(v_tablename,v_key_col);
  v_debug   :='Processed sequence';
  IF v_trigname IS NOT NULL THEN
    EXECUTE IMMEDIATE v_trig;
  END IF;
  v_debug:='Processed trigger';
  EXECUTE IMMEDIATE 'COMMENT ON TABLE '||v_tablename||' IS '''||v_comment||'''';
  v_debug:='Processed comment';
  -- If the geometype is not NULL then metadata is requried and the table will be created empty.
  -- If the geomtype is NULL then whatever process populates the table is responsible for the metadata creation.
  IF v_geomtype IS NOT NULL THEN
      CASE v_geomtype
        WHEN 'P' THEN
          v_geomtype_in:='10';
        WHEN 'L' THEN
          v_geomtype_in:='1';
        WHEN 'A' THEN
          v_geomtype_in:='2';
        WHEN 'C' THEN
          v_geomtype_in:='3';
        ELSE
          v_geomtype_in:= NULL;
      END CASE;
      IF v_keytype='I' THEN
        GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,'D',v_geomtype_in);
        ELSIF v_keytype='R' THEN
        GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,'D',v_geomtype_in,'REAL');
        ELSE
        GOOM.SetGDOSYSMetadata(v_tablename,NULL,'D',v_geomtype_in);
      END IF;
      GOOM.SetMBRProj(v_tablename);
      GOOM.SpatialIndex(v_tablename);
      v_debug:='Processed metadata';
  END IF;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,v_sql,v_debug,sqlcode,sqlerrm);
  END CreateTestTable;
-- ----------------------------------------------------------------------------------------
  PROCEDURE Insert_TestTable (v_tablename IN VARCHAR2, 
                               v_tabtype IN VARCHAR2 DEFAULT 'L', 
                               v_keytype IN VARCHAR2 DEFAULT 'I',
                              v_comments IN VARCHAR2 DEFAULT 'Simple Default table',
                                  v_geom IN MDSYS.SDO_GEOMETRY DEFAULT NULL) 
  IS
  c_cmdname     VARCHAR2(32):='Insert_TestTable';
  v_sql         VARCHAR2(2048);
  v_seqname     VARCHAR2(61);
  v_pkey        VARCHAR2(30);
  v_seqflag     NUMBER(1,0);
  v_rdate       DATE;
  v_rflag       NUMBER;
  v_bflag       NUMBER(1,0);
  v_rtext       VARCHAR2(32);
  v_rnum1       NUMBER(5,2);
  v_rnum2       NUMBER(10,0);
  v_rnum3       NUMBER(16,3);
  v_rnum4       NUMBER(38);
  v_rnum5       FLOAT;
  v_rnum6       INTEGER;
  v_longComment CLOB;
  v_debug       VARCHAR2(32);
  BEGIN
  v_pkey:=GOOM.GetKeyCol(v_tablename);
  v_seqname:=GOOM.GetSequenceName(v_tablename,v_pkey);
  CASE v_keytype
   WHEN 'C' THEN
    v_seqflag:=0;
   WHEN 'S' THEN
    v_seqflag:=0;
   ELSE
    v_seqflag:=1;
  END CASE;
  CASE v_tabtype
  WHEN 'L' THEN
    v_rdate:=RDATE(2000);
    v_rflag:=VAL(1,50);
    v_rtext:=STRING('X',32);
    v_rnum1:=VAL*GDODATA.VAL(10,100);
    v_rnum2:=VAL*GDODATA.VAL(10,100);
    v_rnum3:=VAL*GDODATA.VAL(10,10000);
    v_rnum4:=VAL*GDODATA.VAL(10,10000);
    v_rnum5:=VAL*GDODATA.VAL(100,100000);
    v_rnum6:=VAL*GDODATA.VAL(100,100000);
    v_longComment:='This is a CLOB field that contain a large amount of text.  In GDO, this is converted to a Memo field.';
    IF v_rflag>26 THEN
       v_bflag:=0;
    ELSE
       v_bflag:=1;
    END IF;
    IF v_seqflag = 1 THEN
      v_sql:='INSERT INTO '||v_tablename||' VALUES('||v_seqname||'.nextval,'''||sysdate||''','''||v_rdate||''','''||v_longComment||''','''||v_comments||''','''||v_rtext||''','''||v_rnum1||''','''||v_rnum2||''','''||v_rnum3||''','''||v_rnum4||''','''||v_rnum5||''','''||v_rnum6||''','''||v_bflag||''',:geom)';
      EXECUTE IMMEDIATE v_sql USING v_geom;
    ELSE
      v_sql:='INSERT INTO '||v_tablename||' VALUES(NULL,'''||sysdate||''','''||v_rdate||''','''||v_longComment||''','''||v_comments||''','''||v_rtext||''','''||v_rnum1||''','''||v_rnum2||''','''||v_rnum3||''','''||v_rnum4||''','''||v_rnum5||''','''||v_rnum6||''','''||v_bflag||''',:geom)';
      EXECUTE IMMEDIATE v_sql USING v_geom;
    END IF;
  WHEN 'A' THEN
    v_rdate:=RDATE(2000);
    v_rflag:=VAL(1,50);
    v_rtext:=STRING('X',32);
    v_rnum1:=VAL*GDODATA.VAL(10,100);
    v_rnum2:=VAL*GDODATA.VAL(10,100);
    v_rnum3:=VAL*GDODATA.VAL(10,10000);
    v_rnum4:=VAL*GDODATA.VAL(10,10000);
    v_rnum5:=VAL*GDODATA.VAL(100,100000);
    v_rnum6:=VAL*GDODATA.VAL(100,100000);
    v_longComment:='This is a CLOB field that contain a large amount of text.  In GDO, this is converted to a Memo field.';
    IF v_rflag>26 THEN
       v_bflag:=0;
    ELSE
       v_bflag:=1;
    END IF;
    IF v_seqflag = 1 THEN
      v_sql:='INSERT INTO '||v_tablename||' VALUES('||v_seqname||'.nextval,'''||sysdate||''','''||v_rdate||''','''||v_longComment||''','''||v_comments||''','''||v_rtext||''','''||v_rnum1||''','''||v_rnum2||''','''||v_rnum3||''','''||v_rnum4||''','''||v_rnum5||''','''||v_rnum6||''','''||v_bflag||''')';
      EXECUTE IMMEDIATE v_sql;
    ELSE
      v_sql:='INSERT INTO '||v_tablename||' VALUES(NULL,'''||sysdate||''','''||v_rdate||''','''||v_longComment||''','''||v_comments||''','''||v_rtext||''','''||v_rnum1||''','''||v_rnum2||''','''||v_rnum3||''','''||v_rnum4||''','''||v_rnum5||''','''||v_rnum6||''','''||v_bflag||''')';
      EXECUTE IMMEDIATE v_sql;
    END IF;
  ELSE
    IF v_seqflag = 1 THEN
      v_sql:='INSERT INTO '||v_tablename||' VALUES('||v_seqname||'.nextval,'''||v_comments||''',:geom)';
      EXECUTE IMMEDIATE v_sql USING v_geom;
    ELSE
      v_sql:='INSERT INTO '||v_tablename||' VALUES(NULL,'''||v_comments||''',:geom)';
      EXECUTE IMMEDIATE v_sql USING v_geom;
    END IF;
  END CASE;
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,v_sql,'NONE',sqlcode,sqlerrm);
  END Insert_TestTable;
-- ------------------------------------------------------------------------------------
--  Main Procedures for generating tables populated with data.
-- 
  PROCEDURE GenerateAttributeTable(v_tablename IN VARCHAR2,
                                   v_rows      IN INTEGER  DEFAULT 500,
                                   v_tabtype   IN VARCHAR2 DEFAULT 'A',
                                   v_keytype   IN VARCHAR2 DEFAULT 'I')
  IS
      c_cmdname   VARCHAR2(32):='GenerateAttributeTable';
      v_seqname   VARCHAR2(61);
      v_pkey      VARCHAR2(30);
      v_comments  VARCHAR2(255):='Random Generated Attribute Data';
      v_commit    INTEGER:=1;
      v_count     INTEGER;
  BEGIN
    CreateTestTable(v_tablename,v_tabtype,v_keytype);
    INIT(TO_CHAR(SYSDATE,'MM-DD-YYYY HH24:MI:SS')||USER);
    v_pkey:=GOOM.GetKeyCol(v_tablename);
    v_seqname:=GOOM.GetSequenceName(v_tablename,v_pkey);
    FOR I IN 1..v_rows LOOP
        v_comments:='Random Comment: '||I;
        v_commit:=v_commit+1;
        Insert_TestTable(v_tablename,v_tabtype,v_keytype,v_comments);
        IF v_commit>5000 THEN
         COMMIT;
         v_commit:=1;
        END IF;
    END LOOP;
    COMMIT;
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM '||v_tablename INTO v_count;
    GOOM.Response('CREATED TABLE',v_tablename||' - Rows requested:'||v_rows||' Rows inserted:'||v_count||'.');
      IF v_keytype='I' THEN
        GOOM.SetGDOSYSMetadata(v_tablename,v_seqname);
      ELSIF v_keytype='R' THEN
        GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,NULL,NULL,NULL,'REAL');
      ELSE
        GOOM.SetGDOSYSMetadata(v_tablename);
      END IF;
  EXCEPTION
     WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_tablename,v_rows||':'||v_tabtype||':'||v_keytype,sqlcode,sqlerrm);
  END GenerateAttributeTable;
-- -------------------------------------------------------- 
  PROCEDURE GeneratePtTable(v_tablename IN VARCHAR2,
                            v_rows      IN INTEGER  DEFAULT 500,
                            v_tabtype   IN VARCHAR2 DEFAULT 'L',
                            v_keytype   IN VARCHAR2 DEFAULT 'I',
                            v_type      IN VARCHAR2 DEFAULT 'O',
                            v_xlo       IN NUMBER   DEFAULT 0,
                            v_ylo       IN NUMBER   DEFAULT 0,
                            v_xhi       IN NUMBER   DEFAULT 100000,
                            v_yhi       IN NUMBER   DEFAULT 100000,
                            v_dim       IN INTEGER  DEFAULT 3) 
  IS
      c_cmdname   VARCHAR2(32):='GeneratePtTable';
      v_seqname   VARCHAR2(128);
      v_pkey      VARCHAR2(61);
      v_gtype     INTEGER:=10;
      v_geometry  VARCHAR2(32):='GEOMETRY';
      v_comments  VARCHAR2(255):='Random Generated using New USA CS';
      v_cs        VARCHAR2(64):='D'; -- Assign Default CS
      v_commit    INTEGER:=1;
      v_rotation  NUMBER;
      v_geom      MDSYS.SDO_GEOMETRY;
      v_count     INTEGER;
      v_debug     VARCHAR2(32):='Init';
  --
  BEGIN 
    CreateTestTable(v_tablename,v_tabtype,v_keytype);
    v_debug  :='TestTable: '||v_tablename;
    INIT(TO_CHAR(SYSDATE,'MM-DD-YYYY HH24:MI:SS')||USER);
    v_pkey   :=GOOM.GetKeyCol(v_tablename);
    v_debug  :='PKey: '||v_pkey;
    v_seqname:=GOOM.GetSequenceName(v_tablename,v_pkey);
    v_debug  :='Sequence Created';
    FOR I IN 1..v_rows LOOP
        v_debug:='In Loop: '||I;
        v_rotation:=VAL(0,360);
        v_comments:='Random point: '||I;
        v_commit:=v_commit+1;
        v_geom:=RANDOM_PT(v_xlo,v_xhi,v_ylo,v_yhi,v_rotation,v_dim,v_type);
        Insert_TestTable(v_tablename,v_tabtype,v_keytype,v_comments,v_geom);
        IF v_commit>5000 THEN
         COMMIT;
         v_commit:=1;
        END IF;
    END LOOP;
    COMMIT;
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM '||v_tablename INTO v_count;
    GOOM.Response(c_cmdname,v_tablename||' - Points requested:'||v_rows||' Points inserted:'||v_count||'.');
    GOOM.SetMBRProj(v_tablename);
    GOOM.RTree(v_tablename);
      IF v_keytype='I' THEN
        GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,v_cs);
        ELSIF v_keytype='R' THEN
        GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,v_cs,v_gtype,NULL,'REAL');
        ELSE
        GOOM.SetGDOSYSMetadata(v_tablename);
      END IF;
  EXCEPTION
     WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_tablename,v_tabtype||':'||v_keytype||':'||v_type,v_debug,sqlerrm);
  END GeneratePtTable; 
-- --------------------------------------------------------
  PROCEDURE GeneratePolyTable(v_tablename IN VARCHAR2,
                              v_rows      IN INTEGER  DEFAULT 500,
                              v_tabtype   IN VARCHAR2 DEFAULT 'L',
                              v_keytype   IN VARCHAR2 DEFAULT 'I',
                              v_xlo       IN NUMBER   DEFAULT 0,
                              v_ylo       IN NUMBER   DEFAULT 0,
                              v_xhi       IN NUMBER   DEFAULT 100000,
                              v_yhi       IN NUMBER   DEFAULT 100000,
                              v_scalelo   IN INTEGER  DEFAULT 10,
                              v_scalehi   IN INTEGER  DEFAULT 20,
                              v_dim       IN INTEGER  DEFAULT 2,
                              v_polytyp   IN INTEGER  DEFAULT 1)                  
  IS
  c_cmdname   VARCHAR2(32):='GeneratePolyTable';
  --
  v_seqname   VARCHAR2(61);
  v_geometry  VARCHAR2(32):='GEOMETRY';
  v_gtype     INTEGER:=2;
  v_comments  VARCHAR2(255):='Randomly Generated Polys';
  v_cs        VARCHAR2(64):='D'; -- Assign Default CS
  v_pkey      VARCHAR2(30);
  v_scale     NUMBER;
  v_geom      MDSYS.SDO_GEOMETRY;
  v_text      VARCHAR2(32);
  v_commit    INTEGER:=1;
  v_maxcommit INTEGER:=10000;
  v_count     INTEGER;
  --
  BEGIN
    CreateTestTable(v_tablename,v_tabtype,v_keytype);
    INIT(TO_CHAR(SYSDATE,'MM-DD-YYYY HH24:MI:SS')||USER);
    v_pkey:=GOOM.GetKeyCol(v_tablename);
    v_seqname:=GOOM.GetSequenceName(v_tablename,v_pkey);
    IF v_dim=2 THEN
     v_comments:='Randomly Generated 2D Polys';
    ELSE
     v_comments:='Randomly Generated 3D Polys';
    END IF;
    IF v_polytyp <> 1 THEN
      v_maxcommit := 50;
    END IF;
    FOR J IN 1..v_rows LOOP
      v_commit:=v_commit+1;
      v_scale:=GDODATA.VAL(v_scalelo,v_scalehi);
      v_geom:=GDODATA.Random_Poly(v_scale,v_dim,v_xlo,v_xhi,v_ylo,v_yhi,v_polytyp);
      GDODATA.Insert_TestTable(v_tablename,v_tabtype,v_keytype,v_comments,v_geom);
      IF v_commit>v_maxcommit THEN
        COMMIT;
        v_commit:=1;
      END IF;
    END LOOP;
    COMMIT;
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM '||v_tablename INTO v_count;
    GOOM.Response(c_cmdname,v_tablename||' - Polys requested:'||v_rows||' Polys inserted:'||v_count||'.');
    GOOM.SetMBRProj(v_tablename);
    GOOM.RTree(v_tablename);
    IF v_keytype='I' THEN
      GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,v_cs);
    ELSIF v_keytype='R' THEN
      GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,v_cs,v_gtype,NULL,'REAL');
    ELSE
      GOOM.SetGDOSYSMetadata(v_tablename);
    END IF;
  EXCEPTION
     WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_tablename,v_tabtype||':'||v_keytype,sqlcode,sqlerrm);
  END GeneratePolyTable;
-- --------------------------------------------------------
  PROCEDURE GenerateLineTable(v_tablename IN VARCHAR2,
                              v_rows      IN VARCHAR2 DEFAULT 500,
                              v_tabtype   IN VARCHAR2 DEFAULT 'L',
                              v_keytype   IN VARCHAR2 DEFAULT 'I',
                              v_maxver    IN VARCHAR2 DEFAULT 25,
                              v_varhi     IN INTEGER  DEFAULT 100,
                              v_xlo       IN NUMBER   DEFAULT 0,
                              v_ylo       IN NUMBER   DEFAULT 0,
                              v_xhi       IN NUMBER   DEFAULT 100000,
                              v_yhi       IN NUMBER   DEFAULT 100000,
                              v_dim       IN INTEGER  DEFAULT 2)  
  IS
      c_cmdname   VARCHAR2(32):='GenerateLineTable';
  --
      v_seqname   VARCHAR2(61);
      v_gtype     INTEGER:=1;
      v_varlo     NUMBER:=10;
      v_comments  VARCHAR2(255);
      v_pkey      VARCHAR2(30);
      v_count     INTEGER;
      v_cs        VARCHAR2(64):='D';
      v_geom      MDSYS.SDO_GEOMETRY;
      v_geometry  VARCHAR2(32):='GEOMETRY';
      v_commit    INTEGER:=1;
  BEGIN 
  -- ------------------------------------------------------------------------------------
    CreateTestTable(v_tablename,v_tabtype,v_keytype);
    INIT(TO_CHAR(SYSDATE,'MM-DD-YYYY HH24:MI:SS')||USER);
    v_pkey:=GOOM.GetKeyCol(v_tablename);
    v_seqname:=GOOM.GetSequenceName(v_tablename,v_pkey);
    FOR I IN 1..v_rows LOOP
      v_comments:='Linear test data generation. Line: '||I;
      v_commit:=v_commit+1;
      v_geom:=RANDOM_LINE(v_xlo, v_xhi, v_ylo, v_yhi, v_varlo, v_varhi,v_maxver,v_dim);
      Insert_TestTable(v_tablename,v_tabtype,v_keytype,v_comments,v_geom);
      IF v_commit>5000 THEN
        COMMIT;
        v_commit:=1;
      END IF;
    END LOOP;
    COMMIT;
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM '||v_tablename INTO v_count;
    GOOM.Response(c_cmdname,v_tablename||' - Lines requested:'||v_rows||' Lines inserted:'||v_count||'.');
    GOOM.SetMBRProj(v_tablename);
    GOOM.SpatialIndex(v_tablename);
      IF v_keytype='I' THEN
        GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,v_cs);
        ELSIF v_keytype='R' THEN
        GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,v_cs,v_gtype,NULL,'REAL');
        ELSE
        GOOM.SetGDOSYSMetadata(v_tablename);
      END IF;
  EXCEPTION
     WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_tablename,v_tabtype||':'||v_keytype,sqlcode,sqlerrm);
  END GenerateLineTable;
-- --------------------------------------------------------
  PROCEDURE GenerateCircleTable(v_tablename IN VARCHAR2,
                                v_rows      IN INTEGER  DEFAULT 500,
                                v_tabtype   IN VARCHAR2 DEFAULT 'L',
                                v_keytype   IN VARCHAR2 DEFAULT 'I',
                                v_xlo       IN NUMBER   DEFAULT 0,
                                v_ylo       IN NUMBER   DEFAULT 0,
                                v_xhi       IN NUMBER   DEFAULT 100000,
                                v_yhi       IN NUMBER   DEFAULT 100000,
                                v_diamlo    IN NUMBER   DEFAULT 200,
                                v_diamhi    IN NUMBER   DEFAULT 1200) 
  IS
  c_cmdname   VARCHAR2(32):='GenerateCircleTable';
  --
  v_count     INTEGER;
  v_gtype     INTEGER:=2;
  v_seqname   VARCHAR2(61);
  v_comments  VARCHAR2(255):='Random Generated using New USA CS';
  v_cs        VARCHAR2(64):='D'; -- Assign Default CS
  v_geometry  VARCHAR2(30):='GEOMETRY';
  v_pkey      VARCHAR2(30);
  v_polynum   NUMBER;
  v_diameter  NUMBER;
  v_commit    INTEGER:=1;
  v_geom      MDSYS.SDO_GEOMETRY;
  -- initial coords
  v_Xorigin    NUMBER:=0;
  v_Yorigin    NUMBER:=0;  
  -- Coordinates to load
  v_Xa	FLOAT;
  v_Ya	FLOAT;  
  v_Xb	FLOAT; 
  v_Yb	FLOAT; 
  v_Xc	FLOAT; 
  v_Yc	FLOAT; 
  v_Xd	FLOAT;   
  v_Yd	FLOAT;    
  --
  BEGIN
    GDODATA.CreateTestTable(v_tablename,v_tabtype,v_keytype);
    GDODATA.INIT(TO_CHAR(SYSDATE,'MM-DD-YYYY HH24:MI:SS')||USER);
    v_pkey:=GOOM.GetKeyCol(v_tablename);
    v_seqname:=GOOM.GetSequenceName(v_tablename,v_pkey);
    FOR I IN 1..v_rows LOOP
        v_comments:='Random Circle: '||I;
        -- Generate the required random data
        v_diameter:=GDODATA.VAL(v_diamlo,v_diamhi) ;
        v_xorigin:=GDODATA.VAL(v_xlo,v_xhi) ;
        v_yorigin:=GDODATA.VAL(v_ylo,v_yhi) ;
        v_commit:=v_commit+1;
        -- Calculate new poly coordinates in X and Y
        v_Xa:=v_Xorigin;
        v_Ya:=v_Yorigin;
        v_Xb:=v_Xorigin + (v_diameter/2) ;
        v_Yb:=v_Yorigin - (v_diameter/2) ;
        v_Xc:=v_Xorigin + (v_diameter) ;
        v_Yc:=v_Yorigin ;
        v_Xd:=v_Xorigin + (v_diameter/2);
        v_Yd:=v_Yorigin + (v_diameter/2) ;
        v_geom:=MDSYS.SDO_GEOMETRY(2003, NULL, NULL,
                MDSYS.SDO_ELEM_INFO_ARRAY(1,1005,2,1,2,2, 5,2,2),
                MDSYS.SDO_ORDINATE_ARRAY(v_Xa,v_Ya,v_Xb,v_Yb,v_Xc,v_Yc,v_Xd,v_Yd,v_Xa,v_Ya));
        GDODATA.Insert_TestTable(v_tablename,v_tabtype,v_keytype,v_comments,v_geom);
        IF v_commit>5000 THEN
          COMMIT;
          v_commit:=1;
        END IF;
    END LOOP;
    COMMIT;
    EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM '||v_tablename INTO v_count;
    GOOM.Response(c_cmdname,v_tablename||' - Circles requested:'||v_rows||' Circles inserted:'||v_count||'.');
    GOOM.SetMBRProj(v_tablename);
    GOOM.SpatialIndex(v_tablename);
    IF v_keytype='I' THEN
      GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,v_cs);
    ELSIF v_keytype='R' THEN
      GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,v_cs,v_gtype,NULL,'REAL');
    ELSE
      GOOM.SetGDOSYSMetadata(v_tablename);
    END IF;
  EXCEPTION
     WHEN OTHERS THEN
       GOOM.REPORT_ERROR (c_cmdname,v_tablename,v_tabtype||':'||v_keytype,sqlcode,sqlerrm);
  END GenerateCircleTable;
-- --------------------------------------------------------
  PROCEDURE GenerateReferenceGrid(v_tablename IN VARCHAR2,
                                  v_xlo       IN NUMBER DEFAULT 0,
                                  v_ylo       IN NUMBER DEFAULT 0,
                                  v_xhi       IN NUMBER DEFAULT 100000,
                                  v_yhi       IN NUMBER DEFAULT 100000,
                                  v_divx      IN NUMBER DEFAULT 1000,
                                  v_divy      IN NUMBER DEFAULT 1000)
  IS
  c_cmdname   VARCHAR2(32):='GenerateReferenceGrid';
  --
  v_geomcol  VARCHAR2(30);
  v_commit    INTEGER:=1;
  v_maxcommit INTEGER:=5000;
  v_seqname  VARCHAR2(61);
  v_cs       VARCHAR2(64):='D';
  v_xoffset  FLOAT;
  v_yoffset  FLOAT;
  v_results  MDSYS.SDO_GEOMETRY;
  v_sql      VARCHAR2(1024);
  v_areaID   VARCHAR2(32);
  v_name     VARCHAR2(32):='NONE';
  v_comments VARCHAR2(32):='NONE';
  v_count    INTEGER;
  v_x1       FLOAT;
  v_y1       FLOAT;
  v_x2       FLOAT;
  v_y2       FLOAT;
  v_x3       FLOAT;
  v_y3       FLOAT;
  v_x4       FLOAT;
  v_y4       FLOAT;
  v_xo1      FLOAT;
  v_yo1      FLOAT;
  v_xo2      FLOAT;
  v_yo2      FLOAT;
  v_xo3      FLOAT;
  v_yo3      FLOAT;
  v_xo4      FLOAT;
  v_yo4      FLOAT;
  BEGIN
   --
   GOOM.DropTable(UPPER(v_tablename));
   EXECUTE IMMEDIATE 'CREATE TABLE '||v_tablename||'(PID INTEGER PRIMARY KEY, CELL_IDENTIFIER VARCHAR2(32),CELL_NAME VARCHAR2(32),COMMENTS VARCHAR2(255),CREATION_DATE DATE, GRID_CELL MDSYS.SDO_GEOMETRY)';
   GOOM.CreateSequence(v_tablename,'PID');
   v_seqname:=GOOM.GetSequenceName(v_tablename,'PID');
   --
   v_xoffset:=ABS(v_xhi-v_xlo)/v_divx;
   v_yoffset:=ABS(v_yhi-v_ylo)/v_divy;
   v_x1:=0;
   v_y1:=0;
   v_x2:=v_xoffset;
   v_y2:=0;
   v_x3:=v_xoffset;
   v_y3:=v_yoffset;
   v_x4:=0;
   v_y4:=v_yoffset;
   FOR I IN 1..v_divx  LOOP
     FOR J IN 1..v_divy  LOOP
       v_commit:=v_commit+1;
       v_areaID:=TO_CHAR('X'||I||':'||'Y'||J);
       v_xo1:=v_xlo+(I-1)*v_xoffset+v_x1 ;
       v_yo1:=v_ylo+(J-1)*v_yoffset+v_y1 ;
       v_xo2:=v_xlo+(I-1)*v_xoffset+v_x2 ;
       v_yo2:=v_ylo+(J-1)*v_yoffset+v_y2 ;
       v_xo3:=v_xlo+(I-1)*v_xoffset+v_x3 ;
       v_yo3:=v_ylo+(J-1)*v_yoffset+v_y3 ;
       v_xo4:=v_xlo+(I-1)*v_xoffset+v_x4 ;
       v_yo4:=v_ylo+(J-1)*v_yoffset+v_y4 ;
       v_results:=MDSYS.SDO_GEOMETRY(2003, NULL, NULL,MDSYS.SDO_ELEM_INFO_ARRAY(1,1003,1),MDSYS.SDO_ORDINATE_ARRAY(v_xo1,v_yo1,v_xo2,v_yo2,v_xo3,v_yo3,v_xo4,v_yo4,v_xo1,v_yo1));
       v_sql:='INSERT INTO '||v_tablename||' VALUES('||v_seqname||'.nextval,'''||v_areaID||''','''||v_name||''','''||v_comments||''','''||sysdate||''',:geom)';
       EXECUTE IMMEDIATE v_sql USING v_results;
     END LOOP;
      IF v_commit>v_maxcommit THEN
        COMMIT;
        v_commit:=1;
      END IF;
   END LOOP;
   COMMIT;
   EXECUTE IMMEDIATE 'SELECT COUNT(*) FROM '||v_tablename INTO v_count;
   GOOM.Response(c_cmdname,v_tablename||' - Cells requested:'||v_divx||'X'||v_divy||'. Cells inserted:'||v_count||'.');
   v_geomcol:=GOOM.GetGeom(v_tablename);
   GOOM.SetMBRProj(v_tablename);
   GOOM.SetGDOSYSMetadata(v_tablename,v_seqname,v_cs);
   GOOM.SpatialIndex(v_tablename);
  EXCEPTION
    WHEN OTHERS THEN
      GOOM.REPORT_ERROR (c_cmdname,v_tablename,v_areaID,sqlcode,sqlerrm);
  END GenerateReferenceGrid;
-- --------------------------------------------------------
-- Quickly make a table visible in GeoMedia.
  PROCEDURE FastFeatureClass(v_tablename IN VARCHAR2) IS
  BEGIN
   GOOM.SETMBRPROJ(v_tablename);
   GOOM.SpatialIndex(v_tablename);
   GOOM.SetGDOSYSMetadata(v_tablename);
  END FastFeatureClass;
-- ----------------------------------------------------------------------------------------
  PROCEDURE HELPME(v_help IN VARCHAR2 DEFAULT NULL) IS
  BEGIN
  GOOM.TitleBlock('GDODATA Generation Package Help');
  IF UPPER(v_help) = 'ALL' THEN
  GOOM.TitleLine('Random Attribute Functions');
  GOOM.DashLine;
  GOOM.DBMSG('--  PROCEDURE INIT (v_SEED IN VARCHAR2);');
  GOOM.DBMSG('--             Initialize the random number generator');
  GOOM.DotLine;
  GOOM.DBMSG('--  PROCEDURE ShowASCII;');
  GOOM.DBMSG('--             Display the ASCII Character codes');
  GOOM.DotLine;
  GOOM.DBMSG('--  FUNCTION VAL RETURN NUMBER;   ');
  GOOM.DBMSG('--             result:=GDODATA.val;');
  GOOM.DBMSG('--             Get a random Oracle number x, 0.0 <= x < 1.0');
  GOOM.DotLine;
  GOOM.DBMSG('--  FUNCTION VAL (v_LOW IN NUMBER, v_HIGH IN NUMBER) RETURN NUMBER;');
  GOOM.DBMSG('--             result:=GDODATA.val(-180,180);');
  GOOM.DBMSG('--             Get a random Oracle number x, LOW <= x < HIGH');
  GOOM.DotLine;
  GOOM.DBMSG('--  FUNCTION NORM RETURN NUMBER;');
  GOOM.DBMSG('--             result:=GDODATA.NORM;');
  GOOM.DBMSG('--             Get a random number from a normal distribution');
  GOOM.DotLine;
  GOOM.DBMSG('--  FUNCTION RDATE (v_days IN NUMBER)RETURN DATE;');
  GOOM.DBMSG('--             rdate:=GDODATA.rdate(10);');
  GOOM.DBMSG('--             Get a random date based on number of days from today');
  GOOM.DBMSG('--             + num is future date, - num is past date.');
  GOOM.DotLine;
  GOOM.DBMSG('--  FUNCTION STRING (v_OPT CHAR, v_LEN NUMBER)');
  GOOM.DBMSG('--             rstring:=GDODATA.STRING(''X'',60);');
  GOOM.DBMSG('--             Get a random string of characters, of <v_LEN> (MAX 255)');
  GOOM.DBMSG('--             <v_OPT> specifies that the returned string may contain:');
  GOOM.DBMSG('--             ''u'',''U''  :  upper case alpha characters only');
  GOOM.DBMSG('--             ''l'',''L''  :  lower case alpha characters only');
  GOOM.DBMSG('--             ''a'',''A''  :  alpha characters only (mixed case)');
  GOOM.DBMSG('--             ''x'',''X''  :  any alpha-numeric characters (upper)');
  GOOM.DBMSG('--             ''p'',''P''  :  any printable characters');
  GOOM.DashLine;
  GOOM.TitleLine('Random Geometry Functions');
  GOOM.DashLine;
  GOOM.DBMSG('-- FUNCTION Random_PT(v_xlo IN NUMBER DEFAULT 0, ');
  GOOM.DBMSG('--                    v_xhi IN NUMBER DEFAULT 100000, ');
  GOOM.DBMSG('--                    v_ylo IN NUMBER DEFAULT 0, ');
  GOOM.DBMSG('--                    v_yhi IN NUMBER DEFAULT 100000,');
  GOOM.DBMSG('--                    v_rot IN NUMBER DEFAULT NULL,');
  GOOM.DBMSG('--                    v_dim IN NUMBER DEFAULT 3,');
  GOOM.DBMSG('--                    v_typ IN VARCHAR2 DEFAULT ''O'') ');
  GOOM.DBMSG('--                    RETURN MDSYS.SDO_GEOMETRY');
  GOOM.DBMSG('-- Returns a random point geometry in the specified range.');
  GOOM.DBMSG('--    xlo,xhi,ylo,yhi is the range for the pt');
  GOOM.DBMSG('--    v_rot is the rotation angle, NULL generates random rotation');
  GOOM.DBMSG('--    v_dim is dimension - 2 for 2d, 3 for 3d.  Default is 3.');
  GOOM.DBMSG('--    v_typ is point type. G for GeoMedia, O for Oracle 10G');
  GOOM.DBMSG('--    geom:=RANDOM_PT(xlo,xhi,ylo,yhi,v_rot,v_dim,v_typ);');
  GOOM.DBMSG('--    geom:=RANDOM_PT;  To use defaults');
  GOOM.DotLine;
  GOOM.DBMSG('--  FUNCTION Random_POLY(v_scale IN INTEGER DEFAULT 10, ');
  GOOM.DBMSG('--                       v_Xlo in NUMBER DEFAULT 0, ');
  GOOM.DBMSG('--                       v_Xhi in NUMBER DEFAULT 100000,');
  GOOM.DBMSG('--                       v_ylo in NUMBER DEFAULT 0,');
  GOOM.DBMSG('--                       v_yhi in NUMBER DEFAULT 100000) ');
  GOOM.DBMSG('--                       RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;');
  GOOM.DBMSG('-- Returns a random polygon geometry in the specified range.');
  GOOM.DBMSG('--    geom:=RANDOM_POLY(scale,xlo,xhi,ylo,yhi);');
  GOOM.DBMSG('--    xlo,xhi,ylo,yhi is the range for the poly');
  GOOM.DBMSG('--    scale regulates the maximum size of the polygon.');
  GOOM.DBMSG('--    geom:=RANDOM_POLY;  to use defaults');
  GOOM.DotLine;
  GOOM.DBMSG('--  FUNCTION Random_Line(v_xlo IN NUMBER DEFAULT 0, ');
  GOOM.DBMSG('--                       v_xhi in NUMBER DEFAULT 100000, ');
  GOOM.DBMSG('--                       v_ylo in NUMBER DEFAULT 0,');
  GOOM.DBMSG('--                       v_yhi in NUMBER DEFAULT 100000,');
  GOOM.DBMSG('--                       v_varlo in NUMBER DEFAULT 250,');
  GOOM.DBMSG('--                       v_varhi in NUMBER DEFAULT 250,');
  GOOM.DBMSG('--                       v_maxver in NUMBER DEFAULT 25,');
  GOOM.DBMSG('--                       v_dim in NUMBER DEFAULT 3) ');
  GOOM.DBMSG('--                       RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;');
  GOOM.DBMSG('-- Returns a random linear geometry in the specified range.');
  GOOM.DBMSG('--    geom:=RANDOM_LINE(v_xlo,v_xhi,v_ylo,ylo,v_yhi,v_varlo,v_varhi,v_maxver,v_dim);');
  GOOM.DBMSG('--    xlo,xhi,ylo,yhi is the range for the line');
  GOOM.DBMSG('--    v_varlo,v_varhi regulates the variation along the line.');
  GOOM.DBMSG('--    v_maxver specifies the number of vertices.');
  GOOM.DBMSG('--    v_dim specifies the dimension 2D or 3D.');
  GOOM.DBMSG('--    geom:=Random_Line;  to use defaults');
  GOOM.DotLine;
  GOOM.DBMSG('--  FUNCTION LORENZ(v_scale     IN NUMBER DEFAULT 10,     ');
  GOOM.DBMSG('--                  v_iteration IN NUMBER DEFAULT 800,    ');
  GOOM.DBMSG('--                  v_xorigin   IN NUMBER DEFAULT 0,      ');
  GOOM.DBMSG('--                  v_yorigin   IN NUMBER DEFAULT 0 )     ');
  GOOM.DBMSG('--           RETURN MDSYS.SDO_GEOMETRY');
  GOOM.DBMSG('-- Returns a lorenz geometry in the specified range.');
  GOOM.DBMSG('--    geom:=LORENZ(v_scale,v_iteration,v_xorigin,v_yorigin;');
  GOOM.DotLine;
  GOOM.DBMSG('-- FUNCTION SPIROGRAPH(v_scale   IN NUMBER         DEFAULT 10,    ');
  GOOM.DBMSG('--                     v_res     IN NUMBER         DEFAULT 10,    ');
  GOOM.DBMSG('--                     v_cycles  IN NUMBER         DEFAULT 2,    ');
  GOOM.DBMSG('--                     v_Aradius IN NUMBER         DEFAULT 100,    ');
  GOOM.DBMSG('--                     v_Bradius IN NUMBER         DEFAULT 20,    ');
  GOOM.DBMSG('--                     v_param   IN NUMBER         DEFAULT 80,    ');
  GOOM.DBMSG('--                     v_xorigin IN NUMBER         DEFAULT 0,    ');
  GOOM.DBMSG('--                     v_yorigin IN NUMBER         DEFAULT 0)    ');
  GOOM.DBMSG('--                     RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;    ');
  GOOM.DBMSG('-- Returns a SPIROGRAPH line geometry in the specified range. ');
  GOOM.DBMSG('-- v_scale  : size Default 10');
  GOOM.DBMSG('-- v_res    : resolution Default 10');
  GOOM.DBMSG('-- v_cycles : cycles Default 2');
  GOOM.DBMSG('-- v_Aradius: Large Gear Radius Default 100');
  GOOM.DBMSG('-- v_Bradius: Small Gear Radius Default 20');
  GOOM.DBMSG('-- v_param  : Adjustment Param Default 80');
  GOOM.DBMSG('-- v_xorigin: X origin Default 0');
  GOOM.DBMSG('-- v_yorigin: Y origin Default 0');
  GOOM.DotLine;
  GOOM.DBMSG('-- FUNCTION SERPENSKI(v_scale     IN NUMBER DEFAULT 1,     ');
  GOOM.DBMSG('--                    v_iteration IN NUMBER DEFAULT 400,     ');
  GOOM.DBMSG('--                    v_xorigin   IN NUMBER DEFAULT 0,     ');
  GOOM.DBMSG('--                    v_yorigin   IN NUMBER DEFAULT 0 )     ');
  GOOM.DBMSG('--           RETURN MDSYS.SDO_GEOMETRY DETERMINISTIC;');
  GOOM.DBMSG('-- Returns a SERPENSKI line geometry in the specified range.');
  GOOM.DBMSG('--    geom:=SERPENSKI(v_scale,v_iteration,v_xorigin,v_yorigin;');
  GOOM.DotLine;
  GOOM.DBMSG('-- FUNCTION CREATE_POLY(v_poly_type IN INTEGER, ');
  GOOM.DBMSG('--                      v_scale IN INTEGER, ');
  GOOM.DBMSG('--                      v_Xorigin in NUMBER, ');
  GOOM.DBMSG('--                      v_Yorigin in NUMBER) ');
  GOOM.DBMSG('--                      RETURN MDSYS.SDO_GEOMETRY');
  GOOM.DBMSG('-- Returns a pre-defined polygon at scale and location.');
  GOOM.DBMSG('--     v_geom:=CREATE_POLY(polytype, scale, X, Y)');
  GOOM.DBMSG('--     poly_type is the type of polygon 1 - simple, 2 - complex, 3 - holes, 4 - random');
  GOOM.DBMSG('--     v_scale is the polygon scale or size.  100 is a good starting point.');
  GOOM.DBMSG('--     X,Y is the origin of the polygon.');
  GOOM.DashLine;
  GOOM.TitleLine('Table Procedures');
  GOOM.DotLine;
  GOOM.DBMSG('-- PROCEDURE CreateTestTable (v_tablename IN VARCHAR2, ');
  GOOM.DBMSG('--                             v_tabtype  IN VARCHAR2 DEFAULT ''S'',');
  GOOM.DBMSG('--                             v_keytype  IN VARCHAR2 DEFAULT ''I'',');
  GOOM.DBMSG('--                            v_geomtype  IN VARCHAR2 DEFAULT NULL) ');
  GOOM.DBMSG('-- Creates a test table.');
  GOOM.DBMSG('-- v_tablename: the table name to use - 25 characters or less.');
  GOOM.DBMSG('-- v_tabtype  : S creates a simple geometry table. L creates a table with all supported types.');
  GOOM.DBMSG('-- v_keytype  : primary key type. I default integer. C is a character.  R creates Number(38) Key');
  GOOM.DBMSG('-- v_geomtype : the geometry type.  If you are automatically populating the table, this should be NULL.');
  GOOM.DBMSG('--              For a blank table,  enter P for Point, L for Line, A for Area or C for Compound.');
  GOOM.DotLine;
  GOOM.DBMSG('-- PROCEDURE Insert_TestTable (v_tablename IN VARCHAR2, ');
  GOOM.DBMSG('--                            v_tabtype IN VARCHAR2 DEFAULT ''S'',');
  GOOM.DBMSG('--                            v_keytype IN VARCHAR2 DEFAULT ''I'', ');
  GOOM.DBMSG('--                            v_comments IN VARCHAR2 DEFAULT ''Simple Default table'',');
  GOOM.DBMSG('--                            v_geom IN MDSYS.SDO_GEOMETRY) ');
  GOOM.DBMSG('-- Inserts data automatically into the tables created by CreateTestTable.');
  GOOM.DBMSG('-- v_tablename: the table name to use - 25 characters or less.');
  GOOM.DBMSG('-- v_tabtype  : S creates a simple geometry table. L creates a table with all supported types.');
  GOOM.DBMSG('-- v_keytype  : the primary key type. I is the default and is integer. C is a character key.  R creates and Number(38) Key');
  GOOM.DBMSG('-- v_comments : an optional comment for the comments field. Max is 255.');
  GOOM.DBMSG('-- v_geom     : the generated geometry to insert into the row.  All other values are randomly generated.');
  GOOM.DotLine;
  GOOM.DBMSG('-- FUNCTION Geom2PtCluster(v_polygeom in SDO_GEOMETRY);');
  GOOM.DBMSG('-- Creates a point cluster geometry from an input area or linear geometry.');
  END IF;
  GOOM.TitleBlock('Test Data Generators');
  GOOM.DBMSG('-- EXEC GDODATA.GenerateAttributeTable(v_tablename,v_rows,v_tabtype,v_keytype);');
  GOOM.DBMSG('-- EXEC GDODATA.GeneratePtTable(v_tablename,v_rows,v_tabtype,v_keytype,v_type,v_xlo,v_ylo,v_xhi,v_yhi,v_dim);'); 
  GOOM.DBMSG('-- EXEC GDODATA.GenerateLineTable(v_tablename,v_rows,v_tabtype,v_keytype,v_maxver,v_varhi,v_xlo,v_ylo,v_xhi,v_yhi,v_dim);');  
  GOOM.DBMSG('-- EXEC GDODATA.GeneratePolyTable(v_tablename,v_rows,v_tabtype,v_keytype,v_xlo,v_ylo,v_xhi,v_yhi,v_scalelo,v_scalehi,v_dim);');  
  GOOM.DBMSG('-- EXEC GDODATA.GenerateCircleTable(v_tablename,v_rows,v_tabtype,v_keytype,v_xlo,v_ylo,v_xhi,v_yhi,v_diamlo,v_diamhi);'); 
  GOOM.DBMSG('-- Globals:');
  GOOM.DBMSG('--   v_tabtype: L - Large Table (Default),  S - Small Table.');
  GOOM.DBMSG('--   v_keytype: I - Integer key (Default), R - Real key, C - Char Key, S - GUID Key.');
  GOOM.DBMSG('--   v_rows   : Number of records to create. Default 500');
  GOOM.DBMSG('--   v_xlo,v_ylo,v_xhi,v_yhi: Range of data - Lower left corner to upper right corner. Default 0 to 100000');
  GOOM.DBMSG('--   v_dim: Number of dimensions 2 (Default) or 3.');
  GOOM.DBMSG('-- Points Only - v_type : Point Type: N - Native, O - Oracle Oriented Point.  Default O');
  GOOM.DBMSG('-- Lines Only - v_maxver : Maximum number of vertices. Default 25');
  GOOM.DBMSG('--              v_varhi,v_varlo : Range of variation. Defaults 10,100 ');
  GOOM.DBMSG('-- Polys Only - v_scalelo, v_scalehi : Size of polys. Defaults 10, 20');
  GOOM.DBMSG('-- Circles Only - v_diamlo,v_diamhi : Circle Diameter lo and hi. Defaults 200, 1200.');
  GOOM.DBMSG('-- --');
  GOOM.DBMSG('-- For a quick test table with 100 records and an integer key, just pass the tablename:');
  GOOM.DBMSG('--   EXEC GDODATA.GenerateAttributeTable(''MY_TEST_TABLE'',100)');
  GOOM.DBMSG('--   EXEC GDODATA.GeneratePtTable(''MY_TEST_TABLE'',100)');
  GOOM.DBMSG('--   EXEC GDODATA.GenerateLineTable(''MY_TEST_TABLE'',100)');
  GOOM.DBMSG('--   EXEC GDODATA.GeneratePolyTable(''MY_TEST_TABLE'',100)');
  GOOM.DotLine;
  GOOM.DBMSG('-- Reference Grid - Create a grid based on the range and divisions provided.');
  GOOM.DBMSG('-- EXEC GDODATA.GenerateReferenceGrid(v_tablename,v_xlo,v_ylo,v_xhi,v_yhi,v_divx,v_divy);');
  GOOM.DBMSG('--   v_divx,v_divy: Number of divisions in X and Y.  Default 100.');
  GOOM.DotLine;
  GOOM.DBMSG('-- RUN exec gdodata.helpme(''ALL''); -- for for more help.');
  GOOM.DashLine;
  END HELPME;    
-- ----------------------------------------------------------------------------------------
-- This ends the package functions and procedures.  The end statement is required.
-- The INIT serves to initialize the matrix used to calculate the random numbers.
BEGIN
  INIT(TO_CHAR(SYSDATE,'MM-DD-YYYY HH24:MI:SS')||USER);
END GDODATA;
/
SHOW ERRORS
SET SERVEROUTPUT ON;
---------------------------------------------------------------------------------------
PROMPT **..............................................................**;
PROMPT **           IMPORTANT INFORMATION PLEASE READ THIS!            **;
PROMPT **..............................................................**;
PROMPT ** NOTE: Any errors occurring above may leave package unusable. **;
SET TERMOUT OFF
GRANT EXECUTE ON GDOSYS.GDODATA TO PUBLIC;
PROMPT ** NOTE: Privilege on GDODATA Package Granted to PUBLIC.        **;
DROP PUBLIC SYNONYM GDODATA;
CREATE PUBLIC SYNONYM GDODATA FOR GDOSYS.GDODATA;
SET TERMOUT ON
PROMPT ** NOTE: Public synonym GDODATA created.                        **;
PROMPT **..............................................................**;
PROMPT ** To turn on package messages, enter:  SET SERVEROUTPUT ON     **;  
PROMPT ** For version information, enter:      EXEC GDODATA.VERSION    **;
PROMPT ** For Online Help, enter:              EXEC GDODATA.HELPME     **;
PROMPT ** ***************   Installation completed.  ***************** **;
PROMPT ******************************************************************;
EXEC GDODATA.VERSION;
---------------------------------------------------------------------------------------
