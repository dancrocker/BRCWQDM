# Dropbox re-authorization (one-time)

Dropbox now expires access tokens after 4 hours. This replaces the old
`dropb_token.RDS` (a single access token) with a **refresh token**, which
doesn't expire and lets the app mint new access tokens on its own. You only
need to do this once, from your own computer.

## 1. Register a Dropbox app (~5 min)

1. Go to <https://www.dropbox.com/developers/apps> and click **Create app**.
2. Choose **Scoped access**, then **Full Dropbox** (not "App folder" — the
   app's data already lives under `/BRCWQDM` at the root of your Dropbox).
3. Give it any name (e.g. `BRCWQDM`).
4. Open the new app's **Permissions** tab and check these three boxes, then
   click **Submit**:
   - `files.metadata.read`
   - `files.content.read`
   - `files.content.write`
5. On the **Settings** tab, note the **App key** and **App secret** — you'll
   need both in the next step.

## 2. Run the setup script (~2 min)

In R or RStudio, from the project folder, run:

```r
source("funs/dropbox_auth_setup.R")
```

It will:

1. Ask for the App key and App secret from step 1.
2. Open your browser to a Dropbox authorization page — click **Allow**.
3. Ask you to paste the code Dropbox shows you back into R.
4. Save a `dropb_token.RDS` file in the project folder.

## 3. Distribute the new token file

Copy the new `dropb_token.RDS` into `Data/dropb_token.RDS` in the config
bundle you already distribute to each user's install — same file name, same
location as before. No other per-user setup changes.

If anything goes wrong partway through, you can revoke the app's access at
<https://www.dropbox.com/account/connected_apps> and just run the script
again from a clean state.
