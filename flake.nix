{
  outputs = { self, nixpkgs }: {
     nixosConfigurations.nixos-p1 = nixpkgs.lib.nixosSystem {
       system = "x86_64-linux";
       modules = [ ./p1/configuration.nix ];
     };

     devShells.x86_64-linux.default =
       let
         pkgs = nixpkgs.legacyPackages.x86_64-linux;
       in
       pkgs.mkShell {
         buildInputs = [ pkgs.taskwarrior2 ];
         
         shellHook = ''
           export TASKDATA="$PWD/.taskwarrior/data"
           export TASKRC="$PWD/.taskwarrior/taskrc"
           mkdir -p "$TASKDATA"
           
           # Create taskrc if it doesn't exist
           if [ ! -f "$TASKRC" ]; then
             cat > "$TASKRC" << 'TASKRC_EOF'
# Taskwarrior configuration for nixos-config repo
data.location=.taskwarrior/data
default.command=next
verbose=label,new-id
TASKRC_EOF
             echo "Created $TASKRC"
           fi
           
           echo "Taskwarrior configured (data: $TASKDATA)"
         '';
       };
  };
}
