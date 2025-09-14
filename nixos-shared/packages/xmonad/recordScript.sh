readonly AUDIO_FILE="/tmp/kmonad-record-script.wav"
readonly PID_FILE="/tmp/kmonad-record-script.pid"
readonly NOTIFICATION_ID_FILE="/tmp/kmonad-record-script-notification.id"
readonly WINDOW_ID_FILE="/tmp/kmonad-record-script-window.id"

if [[ -z "${OPENAI_API_KEY:-}" ]]; then
    echo "Error: OPENAI_API_KEY environment variable is not set" >&2
    exit 1
fi

get_mic_device() {
    if pactl list sources short | grep -q "usb-046d_HD_Pro_Webcam_C920.*RUNNING"; then
        echo "alsa_input.usb-046d_HD_Pro_Webcam_C920_E285D6EF-02.analog-stereo"
    elif pactl list sources short | grep -q "alsa_input.*sof-hda-dsp.*analog-stereo"; then
        echo "alsa_input.pci-0000_00_1f.3.analog-stereo"
    else
        echo "default"
    fi
}

send_notification() {
    local message="$1"
    local notification_id

    if [[ -f "$NOTIFICATION_ID_FILE" ]]; then
        notification_id=$(cat "$NOTIFICATION_ID_FILE")
        notify-send --replace-id="$notification_id" "Record Script" "$message"
    else
        notification_id=$(notify-send --print-id "Record Script" "$message")
        echo "$notification_id" > "$NOTIFICATION_ID_FILE"
    fi
}

cleanup_temp_files() {
    rm -f "$AUDIO_FILE" "$PID_FILE" "$NOTIFICATION_ID_FILE" "$WINDOW_ID_FILE"
}

activate_recording() {
    rm "${AUDIO_FILE}" || true
    
    # Capture the currently focused window ID
    xdotool getwindowfocus > "$WINDOW_ID_FILE"
    
    local mic_device
    mic_device=$(get_mic_device)
    ffmpeg -y -loglevel quiet -f pulse -i "$mic_device" -codec:a pcm_s16le -ar 16000 -ac 1 "$AUDIO_FILE" &
    echo "$!" > "$PID_FILE"
    send_notification "Started"
}

deactivate_recording() {
    send_notification "Stopping"
    local pid
    pid=$(cat "$PID_FILE")
    kill "$pid" &>/dev/null
    timeout 10s tail --pid="$pid" -f /dev/null || kill -9 "$pid"
    send_notification "Stopped"
}

transcribe_recording() {
    local transcription
    transcription=$(curl -s \
         --fail \
         --request POST \
         --url https://api.openai.com/v1/audio/transcriptions \
         --header "Authorization: Bearer $OPENAI_API_KEY" \
         --header 'Content-Type: multipart/form-data' \
         --form "file=@$AUDIO_FILE" \
         --form model=gpt-4o-transcribe \
         --form prompt="The following is a quick recording of instructions FROM a software developer TO a coding agent or general purpose LLM." \
         | jq -r .text)

    echo "$transcription" | xclip -i -selection clipboard
    send_notification "Copied $(echo "$transcription" | wc -w) words"
    if [[ -n "$transcription" && -f "$WINDOW_ID_FILE" ]]; then
        # Focus the original window and paste the transcription
        local window_id
        window_id=$(cat "$WINDOW_ID_FILE")
        xdotool windowfocus "$window_id"
        sleep 0.1
        # Use primary selection + middle click for reliable Unicode support
        echo -n "$transcription" | xclip -selection primary
        sleep 0.1
        xdotool click --clearmodifiers 2
    fi
    cleanup_temp_files
}

is_recording() {
    [[ -f "$PID_FILE" ]] && kill -0 "$(cat "$PID_FILE")" 2>/dev/null
}

case "$1" in
    activate)
        activate_recording
        ;;
    deactivate)
        deactivate_recording
        transcribe_recording
        ;;
    toggle)
        if is_recording; then
            deactivate_recording
            transcribe_recording
        else
            activate_recording
        fi
        ;;
    *)
        echo "Usage: $0 {activate|deactivate|toggle}"
        exit 1
        ;;
esac
