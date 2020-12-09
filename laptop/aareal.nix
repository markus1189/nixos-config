{ config, pkgs, ...}:

{
  nixpkgs = {
    overlays = [
      (self: super: {
        jmeter = super.callPackage ./jmeter.nix { };
      })
    ];
  };

  containers.aarealkafka = {
    autoStart = false;
    config = { config, pkgs, ... }:
    {
      environment = {
        systemPackages = with pkgs; [ lsof ];
      };

      services = {
        zookeeper = {
          enable = true;
        };

        apache-kafka = {
          enable = true;
          extraProperties = ''
            offsets.topic.replication.factor=1
            delete.topic.enable = true
          '';
        };
      };

      networking = {
        extraHosts = ''
          127.0.0.1 nixos-xps
        '';
      };
    };
  };

  networking = {
    hosts = {
      # Aareal Bank VDI
      "185.255.67.232" = ["swie0vmsec03 swie0vmsec03.aareal.org"];
      "185.255.67.233" = ["swie0vmsec04 swie0vmsec04.aareal.org"];
    };
  };

  environment = {
    systemPackages = with pkgs; [
      (maven.overrideAttrs (old: { jdk = oraclejdk; }))
      openconnect
      jmeter
      postgresql96
    ];

    etc = {
      "aareal/maven-settings.xml".text = ''
        <settings>
          <profiles>
            <profile>
              <repositories>
                <repository>
                  <snapshots>
                    <enabled>false</enabled>
                  </snapshots>
                  <id>artifactory-release</id>
                  <name>libs-release</name>
                  <url>http://lwie0artif01.aareality.aareal.org/artifactory/libs-release</url>
                </repository>
                <repository>
                  <snapshots>
                    <enabled>false</enabled>
                  </snapshots>
                  <id>archetype</id>
                  <name>archetype</name>
                  <url>http://lwie0artif01.aareality.aareal.org/artifactory/libs-release</url>
                </repository>
              </repositories>
              <pluginRepositories>
                <pluginRepository>
                  <snapshots>
                    <enabled>false</enabled>
                  </snapshots>
                  <id>artifactory-release</id>
                  <name>plugins-release</name>
                  <url>http://lwie0artif01.aareality.aareal.org/artifactory/plugins-release</url>
                </pluginRepository>
              </pluginRepositories>
              <id>artifactory</id>
            </profile>
          </profiles>
          <activeProfiles>
            <!--make the profile active all the time -->
            <activeProfile>artifactory</activeProfile>
          </activeProfiles>

          <mirrors>
            <mirror>
              <mirrorOf>*,!artifactory-release</mirrorOf>
              <name>remote-repos</name>
              <url>http://lwie0artif01.aareality.aareal.org/artifactory/remote-repos</url>
              <id>remote-repos</id>
            </mirror>
          </mirrors>
        </settings>
      '';
    };
  };

}
