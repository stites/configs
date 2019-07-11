{
    services.redshift.enable = true;

    # Screen brightness to apply during the day and night, between 0.1 and 1.0. Defaults: "1"
    services.redshift.brightness.day = "0.9";
    services.redshift.brightness.night = "0.6";

    # Additional command-line arguments (:: [string]) to pass to redshift. Default: [ ]
    # Example: [ "-v" "-m randr" ]
    services.redshift.extraOptions = [];

    # # Your current latitude, between -90.0 and 90.0. Must be provided along with longitude. null or string. Default: null
    # services.redshift.latitude

    # # Your current longitude, between -180.0 and 180.0. Must be provided along with latitude. Type: null or string. Default: null
    # services.redshift.longitude

    # The location provider to use for determining your location. If set to manual you must also provide latitude/longitude. If set to geoclue2, you must also enable the global geoclue2 service.
    # Type: one of "manual", "geoclue2".  Default: "manual"
    services.redshift.provider = "geoclue2";


    # Colour temperature to use during the day, between 1000 and 25000 K.  Type: signed integer. Default: 5500
    services.redshift.temperature.day = 5500;

    # Colour temperature to use at night, between 1000 and 25000 K. Type: signed integer. Default: 3700
    services.redshift.temperature.night = 3700;

    # Start the redshift-gtk tray applet. Type: boolean. Default: false
    services.redshift.tray = false;
}
