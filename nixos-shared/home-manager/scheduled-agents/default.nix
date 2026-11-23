{ pkgs, lib, ... }:
let
  claude = "${pkgs.masterPkgs.claude-code}/bin/claude";
  prefix = "scheduled-agent-";
  telegram = "${pkgs.notifySendTelegram}/bin/notifySendTelegram";
  # A preflight exiting non-zero means "nothing to do": the service exits
  # with this, and ExecStopPost stays silent for it.
  skipStatus = 75;

  mkAgent =
    {
      name,
      schedule,
      prompt,
      model ? "opus",
      effort ? "medium",
      permissionMode ? "auto",
      tools ? [ ],
      # Command run before claude. Its stdout is appended to the prompt;
      # lines starting with FAIL are also sent to Telegram.
      preflight ? null,
    }:
    let
      flags = lib.cli.toCommandLineShellGNU { } {
        bg = true;
        name = "${prefix}${name}";
        inherit model effort;
        permission-mode = permissionMode;
        allowedTools = if tools == [ ] then null else lib.concatStringsSep "," tools;
      };
      fullPrompt = prompt + lib.optionalString (preflight != null) ''

        Preflight output:
        @PREFLIGHT@
      '' + ''

        Also write your final answer to
        $(stuff-today --default)/${prefix}${name}-report.md.
      '';
    in
    {
      services."${prefix}${name}" = {
        Unit.Description = "Scheduled agent: ${name}";
        Service = {
          Type = "oneshot";
          ExecStart = "${pkgs.writeShellScript "${prefix}${name}" (
            if preflight == null then
              "${claude} ${flags} ${lib.escapeShellArg fullPrompt}"
            else
              ''
                set -uo pipefail
                out=$(${preflight}); rc=$?
                fails=$(printf '%s\n' "$out" | ${pkgs.gnugrep}/bin/grep '^FAIL' || true)
                [ -n "$fails" ] && ${telegram} "agent ${name}: $fails"
                [ "$rc" -eq 0 ] || exit ${toString skipStatus}
                prompt=${lib.escapeShellArg fullPrompt}
                exec ${claude} ${flags} "''${prompt/@PREFLIGHT@/"$out"}"
              ''
          )}";
          SuccessExitStatus = skipStatus;
          # claude refuses background sessions in an untrusted $HOME.
          WorkingDirectory = "%h/Stuff";
          # The --bg daemon would otherwise die with the oneshot's cgroup.
          KillMode = "process";
          ExecStopPost = "-${pkgs.writeShellScript "${prefix}${name}-notify" ''
            [ "$EXIT_STATUS" = ${toString skipStatus} ] || ${telegram} "agent ${name} dispatched: $SERVICE_RESULT"
          ''}";
          Environment = [ "SSH_AUTH_SOCK=%t/ssh-agent" ];
        };
      };
      timers."${prefix}${name}" = {
        Timer = {
          OnCalendar = schedule;
          Persistent = true;
        };
        Install.WantedBy = [ "timers.target" ];
      };
    };

  jobs = [
    {
      name = "warm-start";
      # the --bg daemon retires sessions idle for 8h
      schedule = "*-*-* 02:30";
      prompt = ''
        You work based on what happened in ~/Stuff (start at llms.txt;
        any host) since your last report
        (scheduled-agent-warm-start-report.md in earlier day dirs; if
        none, the last 3 days). Older material only to follow a thread
        that is still open. Don't repeat items from earlier reports
        unless you have something new.

        Pick a few things you judge most worth my morning, ranked;
        fewer is fine only if the rest is noise.

        Do the legwork now, e.g. web research, filling gaps in notes,
        reviewing new code for high-value improvements (logic,
        performance, bugs).

        Read-only everywhere except today's dir
        ($(stuff-today --default)),
        where you may write drafts, scripts and diffs.

        Never send anything that reaches other people: mail, comments,
        PRs, issues, posts, etc.

        Every item ends in a result, an artifact plus the exact
        command to apply it, or "dropped: tried X, failed because
        Y". No "we could look into X".

        Friction/tooling fixes cost context and maintenance: propose
        one only if it recurred, you verified it against the real
        case, and state its cost vs expected benefit.
      '';
    }
    {
      name = "audiobook-recap";
      schedule = "*-*-* 02:30";
      preflight = "${pkgs.python3}/bin/python3 -I ${./audiobook-recap-preflight.py}";
      prompt = ''
        For each RECAP line below (RECAP <book dir> <old_s> <new_s>
        <total_s> <epub>) dispatch one subagent with this brief and
        the line; ignore FAIL lines. Then send one message,
        ${telegram} "<message>", with each subagent's result line.

        Brief: Append to <book dir>/recap.md a section `## YYYY-MM-DD
        · <chapter at old_s> → <chapter at new_s>` recapping that span
        from the EPUB for a listener who zoned out at times: the major
        plot points, character development and changed relationships,
        new characters and terms, so the next session makes sense.
        Start with a short "Where you were" recap of what came just
        before old_s. Name chapters as the EPUB does, never by audio
        track number.

        Audiobook chapters come from ${pkgs.ffmpeg}/bin/ffprobe
        -show_chapters (several files: one per file; names like
        001.mp3 are not titles); EPUB chapters from the spine
        (${pkgs.unzip}/bin/unzip, ${pkgs.pandoc}/bin/pandoc -t plain),
        aligned by content when counts differ, positions within a
        chapter by word fraction.

        Nothing after new_s, not even hints; don't read past it except
        to find the chapter end. Only after the section is written set
        frontmatter last_position_s = new_s, updated, epub. Touch no
        other file. On failure leave recap.md as is. Return one line:
        title and chapter range, or "FAILED: <reason>".
      '';
    }
  ];
in
{
  systemd.user = lib.mkMerge (map mkAgent jobs);
}
