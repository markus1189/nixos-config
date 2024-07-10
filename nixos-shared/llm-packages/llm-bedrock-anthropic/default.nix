{ lib
, python3
, fetchFromGitHub
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-bedrock-anthropic";
  version = "unstable-2024-07-06";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "sblakey";
    repo = "llm-bedrock-anthropic";
    rev = "c1f8265960887ea6dced945737f549c7e3a55eee";
    hash = "sha256-+GrPCVy5RyAIaZRFpwN0ahqitM9P2wI3ZRO9Nl4EFEA=";
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
