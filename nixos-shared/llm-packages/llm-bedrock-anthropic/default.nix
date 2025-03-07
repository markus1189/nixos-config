{ lib
, python3
, fetchFromGitHub
, source
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-bedrock-anthropic";
  version = "unstable-2025-03-05";
  pyproject = true;

  src = source;

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [
    boto3
    pydantic
    pillow
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
