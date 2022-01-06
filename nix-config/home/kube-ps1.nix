let
  ps1 = builtins.fetchGit {
    url = "https://github.com/jonmosco/kube-ps1.git";
  };
in
{
  files = {
    "per-user/kube-ps1.sh".text = builtins.readFile "${ps1}/kube-ps1.sh";
  };
}
