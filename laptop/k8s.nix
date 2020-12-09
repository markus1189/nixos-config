{config, pkgs, ...}:

{
  environment = {
    variables = {
      KUBECONFIG = [ "$HOME/.kube/config" ];
    };
  };
}
