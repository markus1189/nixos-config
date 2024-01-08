import logging
import datetime
import os
import os.path
import requests
import json
from dateutil import parser

from garminconnect import (
    Garmin,
    GarminConnectConnectionError,
    GarminConnectTooManyRequestsError,
    GarminConnectAuthenticationError,
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

if "GARMIN_CONNECT_START_DATE" in os.environ:
    startdate = parser.parse(os.environ['GARMIN_CONNECT_START_DATE'], fuzzy=True)
else:
    startdate = datetime.date.today() - datetime.timedelta(days = 5)

if "GARMIN_CONNECT_END_DATE" in os.environ:
    enddate = parser.parse(os.environ['GARMIN_CONNECT_END_DATE'], fuzzy=True)
else:
    enddate = datetime.date.today()

logger.info(f"Start: {startdate}, End: {enddate}")

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
        logger.warning("Skipping %s, as it already exists", output_file)
    else:
        logger.info(
            "Downloading %s format as .%s to %s",
            dl_fmt,
            ext,
            output_file)
        data = api.download_activity(activity_id, dl_fmt=dl_fmt)
        with open(output_file, "wb") as fb:
            fb.write(data)

def download_hr(date):
    output_file = f"{target_dir}/{date.isoformat()}_hr.json"
    logger.info(f"Downloading hr data to {output_file}")
    with open(output_file, "wb") as fb:
        data = api.get_heart_rates(date.isoformat())
        fb.write(json.dumps(data).encode())

def download_sleep(date):
    output_file = f"{target_dir}/{date.isoformat()}_sleep.json"
    logger.info(f"Downloading sleep data to {output_file}")
    with open(output_file, "wb") as fb:
        data = api.get_sleep_data(date.date().isoformat())
        fb.write(json.dumps(data).encode())


def download_hrv(date):
    output_file = f"{target_dir}/{date.isoformat()}_hrv.json"
    logger.info(f"Downloading hrv to {output_file}")
    with open(output_file, "wb") as fb:
        data = api.get_hrv_data(date.date().isoformat())
        fb.write(json.dumps(data).encode())


def download_rhr(date):
    output_file = f"{target_dir}/{date.isoformat()}_rhr_day.json"
    logger.info(f"Downloading rhr to {output_file}")
    with open(output_file, "wb") as fb:
        data = api.get_rhr_day(date.date().isoformat())
        fb.write(json.dumps(data).encode())


def download_race_predictions(date):
    output_file = f"{target_dir}/{date.isoformat()}_race_predictions.json"
    logger.info(f"Downloading race predictions to {output_file}")
    with open(output_file, "wb") as fb:
        data = api.get_race_predictions(date.date().isoformat(), date.date().isoformat(), 'daily')
        fb.write(json.dumps(data).encode())


def daterange(start_date, end_date):
    for n in range(int((end_date - start_date).days)):
        yield start_date + datetime.timedelta(n)


try:
    username = os.environ['GARMIN_CONNECT_USER']
    password = os.environ['GARMIN_CONNECT_PASSWORD']
    api = Garmin(username, password)

    logger.info("Logging in as '%s'", username)
    api.login()
    logger.info("Logged in!")

    activities = api.get_activities_by_date(
        startdate.isoformat(),
        enddate.isoformat(),
        None
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

    for single_date in daterange(startdate, enddate):
        download_hr(single_date)
        download_sleep(single_date)
        download_rhr(single_date)
        download_hrv(single_date)
        download_race_predictions(single_date)

except (
        GarminConnectConnectionError,
        GarminConnectAuthenticationError,
        GarminConnectTooManyRequestsError,
) as err:
    logger.error(
        "Error occurred during Garmin Connect communication: %s",
        err,
        exc_info=True)
    raise err

logger.info("Done, signaling healthcheck")
requests.get('https://hc-ping.com/c10412fc-b708-40da-9612-02b60f186a24')
logger.info("FINISHED")
