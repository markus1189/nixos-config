{
  lib,
  python3,
  fetchFromGitHub,
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-deepseek";
  version = "0.1.4";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "abrasumente233";
    repo = "llm-deepseek";
    rev = version;
    hash = "sha256-bYWAKFG5uUB3kgb6rmkqg9dBPcwI4DmZUHzWtDjnggQ=";
  };

  build-system = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  optional-dependencies = {
    test = [
      python3.pkgs.pytest
    ];
  };

  pythonImportsCheck = [
    # "llm_deepseek"
  ];

  dontCheckRuntimeDeps = true;

  meta = {
    description = "Access deepseek.com models via API";
    homepage = "https://github.com/abrasumente233/llm-deepseek";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ ];
  };
}
