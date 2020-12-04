# geoip

Return geographical information associated with an IP address.

It needs the GeoLite2 Country CSV database.

Only IPv4 addresses are currently supported, but patches are welcome.

# Example

```lisp
CL-USER> (geoip:geoip "1.1.1.1")
:AU
"Australia"
#S(GEOIP/GEOIP::GEOLITE2-COUNTRY-BLOCK-IPV4
   :NETWORK "1.1.1.0/24"
   :GEONAME-ID 2077456
   :REGISTERED-COUNTRY-GEONAME-ID 2077456
   :REPRESENTED-COUNTRY-GEONAME-ID NIL
   :IS-ANONYMOUS-PROXY NIL
   :IS-SATELLITE-PROVIDER NIL
   :DECODED-NETWORK (16843008 16843263))
#S(GEOIP/GEOIP::GEOLITE2-COUNTRY-LOCATION
   :GEONAME-ID 2077456
   :LOCALE-CODE "en"
   :CONTINENT-CODE "OC"
   :CONTINENT-NAME "Oceania"
   :COUNTRY-ISO-CODE :AU
   :COUNTRY-NAME "Australia"
   :IS-IN-EUROPEAN-UNION NIL)
```

# License

MIT
