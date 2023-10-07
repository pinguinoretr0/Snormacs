# Sample Snormacs EXWM Setup - TLP
{ config, pkgs, ... }:

{
  # In this import section you can source other Nix files \
  # So if you wanted to you can have all of Snormacs' configuration in an emacs.nix file;
  imports = [ ./hardware-configuration.nix ];

  # Base System
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  networking.hostName = "SnormacsMachine"; 
  networking.networkmanager.enable = true;
  time.timeZone = "America/Los_Angeles";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # EXWM/X11
  services.xserver.enable = true;
  services.xserver.windowManager.exwm.enable = true; 
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  }; system.stateVersion = "NIXOSVERSION"; 
}
