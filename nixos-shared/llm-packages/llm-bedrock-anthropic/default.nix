{ lib
, python3
, fetchFromGitHub
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-bedrock-anthropic";
  version = "unstable-2025-02-02";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "sblakey";
    repo = "llm-bedrock-anthropic";
    rev = "b85319a5f374b92c654378916ebe265fbfff5b67";
    hash = "sha256-9ceuoGjORPH3Om7kkFnOL+d8oyIYKS/IvCFE/oWWhrI=";
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
    description = "Plugin for https://llm.datasette.io/en/stable/ to enable talking with Claude Instant and ClaudeV2 models on AWS Bedrock";
    homepage = "https://github.com/sblakey/llm-bedrock-anthropic";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
    mainProgram = "llm-bedrock-anthropic";
  };
}
