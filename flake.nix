{
  description = "Official Snormacs Nix Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    snromacs.url = "github:thelinuxpirate/Snormacs";
  };

  outputs = {
    self,
    nixpkgs,
    emacs-overlay,
    snormacs
  }:
    {
      packages.x86_64-linux = snormacs.x86_64-linux // {
        emacs = emacs-overlay.overlay.EmacsUnstable;
        ispell = nixpkgs.ispell;
    };

    defaultPackage.x86_64-linux = snormacs.defaultPackage.x86_64-linux;
  };
}
