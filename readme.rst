=====
About
=====
The following code is an Object Pascal based Redis protocol implementation.
The code itself was created for Free Pascal v2.6.0 and above.

=======
License
=======
The project is currently in modified LGPL 3 license. 
I allow any free, open and commercial usage of this library, however any 
changes, I do wish to see return back to the community.

=====
Files
=====
src/
  - rd_protocol.pas - Implementing the protocol itself:
                      http://redis.io/topics/protocol

  - rd_commands.pas - Implementing the commands supported by the database
                      Contains also the parser itself and low level classes
                      per command family

  - rd_types.pas    - Implementing the content types that Redis can have 

  - rd_database.pas - Will implement the High level database class and will 
                      use the above units.
                      You should use this unit for the implementation and not
                      the lower level ones.

tests/
  - test_commands.lpr - Set of tests for rd_commands that check if the commands 
                        works properly or not by the implementation.
                        It does not check all of the commands though.

  
  - test_parser.lpr   - Test the Parser code existed at rc_commands.pas at the 
                        class TRedisParser .
    
  - test_protocol.lpr - Test the implementation of the Socket unit.
                        It uses TRedisIO in a raw usage.

  - test_types.lpr    - Will create tests for the rd_types unit.

  - tes_database.lpr  - Will test the database classes.

tests/unit_tests/
  This directory will contain unit tests created by FPCUnits

docs/
  This directory will contain full fpdoc documentation for everything on this 
  library.

docs/Tutorial
  This directory will contain full examples with documentation inside the source
  code on how to use the following code.

examples/
  This directory will create few simple examples on how things works. 
  It aim will be quick and dirty getting started examples.

============
Dependencies 
============
This version using Synapse using the svn version with the package for Lazarus.
If you are interested in different Socket library, please contact me.

======
Notes:
======
* The following code was created and tested by using Linux 64 bit with FPC 2.6.0
  and Lazarus.

* At the time of creation of this readme file the Redis version that was used to
  test everything is 2.4.6 over the existed documentation on the website, and 
  self testing of the commands, including using wireshark.

* Version 0.1 is located at master trunk. When v0.1 will be baked and ready for 
  usage, there will be a tag that will mark it as that.

* From version 0.2 and beyond, there will be trunks for development. The master 
  trunk will have only the latest release, not the development version.

* Each release will have a tag on the master trunk.


