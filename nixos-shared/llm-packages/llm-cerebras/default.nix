{ lib
, python3
, fetchFromGitHub
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-cerebras";
  version = "unstable-2025-02-02";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "irthomasthomas";
    repo = "llm-cerebras";
    rev = "56fdb9da9e24b4f11419e5b95eb9703b3b2bf449";
    hash = "sha256-mtBtHgzYjqaufqS1C8S26hWyDjJtvrv1wZgP62hss48=";
  };

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [
    boto3
    pydantic
  ];

  pythonImportsCheck = [ ];

  dontCheckRuntimeDeps = true;

  meta = with lib; {
    description = " llm plugin for Cerebras fast inference API ";
    homepage = "https://github.com/irthomasthomas/llm-cerebras";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
    mainProgram = "llm-cerebras";
  };
}
