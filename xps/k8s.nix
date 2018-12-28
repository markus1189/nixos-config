{config, pkgs, ...}:

{
  environment = {
    variables = {
      KUBECONFIG = [ "~/.kube/config" ];
    };
  };
}
