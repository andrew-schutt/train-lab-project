-- Package spec to compensate for scope
-- Hall_IO
-- Cleveland Express Spring 2011
-- Real-Time Embedded Systems
-- Managed by Elliot Schmitt

with Halls; use Halls;


package Hall_IO is

   procedure Sensor_Trig (Hall : in Halls.Hall_ID);

end Hall_IO;
