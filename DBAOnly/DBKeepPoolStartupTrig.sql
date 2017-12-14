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
CREATE OR REPLACE TRIGGER PIN_PROCEDURES_ON_STARTUP
AFTER STARTUP ON DATABASE
BEGIN
   dbms_shared_pool.keep('SYS.STANDARD');                    
   dbms_shared_pool.keep('DBMS_DDL');                    
   dbms_shared_pool.keep('DBMS_DESCRIBE');                                                      
   dbms_shared_pool.keep('DBMS_OUTPUT');                                  
   dbms_shared_pool.keep('DBMS_STANDARD');                    
   dbms_shared_pool.keep('DBMS_UTILITY');                    
   dbms_shared_pool.keep('STANDARD');
   dbms_shared_pool.keep('GDOSYS.GOOM');
   --
   EXECUTE IMMEDIATE 'alter TABLE GDOSYS.GALIASTABLE storage (buffer_pool keep)';
   EXECUTE IMMEDIATE 'alter TABLE GDOSYS.GPARAMETERS storage (buffer_pool keep)';
   EXECUTE IMMEDIATE 'alter TABLE GDOSYS.GEXCLUSIONS storage (buffer_pool keep)';
   EXECUTE IMMEDIATE 'alter TABLE GDOSYS.FIELDLOOKUP storage (buffer_pool keep)';
   EXECUTE IMMEDIATE 'alter TABLE GDOSYS.ATTRIBUTEPROPERTIES storage (buffer_pool keep)';
   EXECUTE IMMEDIATE 'alter TABLE GDOSYS.GEOMETRYPROPERTIES storage (buffer_pool keep)';
   EXECUTE IMMEDIATE 'alter TABLE GDOSYS.GFIELDMAPPING storage (buffer_pool keep)';
   EXECUTE IMMEDIATE 'alter TABLE GDOSYS.GFEATURESBASE storage (buffer_pool keep)';
   EXECUTE IMMEDIATE 'alter TABLE GDOSYS.GINDEX_COLUMNS storage (buffer_pool keep)';
   EXECUTE IMMEDIATE 'alter TABLE GDOSYS.GCOORDSYSTEM storage (buffer_pool keep)';
END;
/

