import logging
import datetime
import os
import os.path
import requests

from garminconnect import (
    Garmin,
    GarminConnectConnectionError,
    GarminConnectTooManyRequestsError,
    GarminConnectAuthenticationError,
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

startdate = datetime.date.today()
enddate = datetime.date.today()

if "GARMIN_CONNECT_TARGET_DIR" in os.environ:
    target_dir = os.environ['GARMIN_CONNECT_TARGET_DIR']
else:
    target_dir = "."


def extension(dl_fmt):
    fmt = str(dl_fmt).split(".")[-1]
    if (fmt == "ORIGINAL"):
        return "zip"
    else:
        return fmt.lower()


def download(activity_id, file_name, dl_fmt):
    ext = extension(dl_fmt)
    output_file = f"{target_dir}/{file_name}.{ext}"
    if os.path.exists(output_file):
        logger.warn("Skipping %s, as it already exists", output_file)
    else:
        logger.info(
            "Downloading %s format as .%s to %s",
            dl_fmt,
            ext,
            output_file)
        data = api.download_activity(activity_id, dl_fmt=dl_fmt)
        with open(output_file, "wb") as fb:
            fb.write(data)


try:
    username = os.environ['GARMIN_CONNECT_USER']
    password = os.environ['GARMIN_CONNECT_PASSWORD']
    api = Garmin(username, password)

    logger.info("Logging in as '%s'", username)
    api.login()
    logger.info("Logged in!")

    activities = api.get_activities_by_date(
        startdate.isoformat(),
        enddate.isoformat(), None
    )

    for activity in activities:
        activity_id = activity["activityId"]
        activity_start = activity["startTimeLocal"]
        activity_type = activity["activityType"]["typeKey"]
        file_name = (activity_start.replace(" ", "_").replace(":", "-")
                     + "_"
                     + activity_type)
        logger.info("Downloading activity with id: %s", activity_id)

        download(activity_id, file_name, api.ActivityDownloadFormat.GPX)
        download(activity_id, file_name, api.ActivityDownloadFormat.TCX)
        download(activity_id, file_name, api.ActivityDownloadFormat.ORIGINAL)
        download(activity_id, file_name, api.ActivityDownloadFormat.CSV)

except (
        GarminConnectConnectionError,
        GarminConnectAuthenticationError,
        GarminConnectTooManyRequestsError,
) as err:
    logger.error(
        "Error occurred during Garmin Connect communication: %s",
        err,
        exc_info=True)

logger.info("Done, signaling healthcheck")
requests.get('https://hc-ping.com/c10412fc-b708-40da-9612-02b60f186a24')
logger.info("FINISHED")
