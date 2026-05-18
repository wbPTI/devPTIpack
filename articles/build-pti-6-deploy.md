# Step 6 — Deploy

> **Note:** The template ships
> [`06-deploy.R`](https://github.com/worldbank/devPTIpack/blob/main/inst/template_pti/06-deploy.R)
> — a plain R script with the `rsconnect::deployApp()` boilerplate
> commented out, **not** sourced by `00-master.R`. This vignette covers
> the same content in tutorial form, plus deployment-target choice and
> post-publish setup.

## What gets deployed

The Posit Connect / shinyapps.io bundle is **`app.R` +
`landing-page.md` + `app-data/`** — nothing else. Specifically excluded:

| Excluded | Why |
|----|----|
| `sample-data/` | Raw inputs — already baked into `app-data/` by Steps 1, 3, 5. |
| `data-raw/` | Generators for the sample data; not needed at runtime. |
| `01-shapes.qmd` … `05-compile.qmd` | Pipeline scripts — already executed; their outputs are in `app-data/`. |
| `06-deploy.R` | This script. Self-evidently not deployed. |
| `docs/` | Optional rendered Quarto trail. If you publish it at all, it goes to GitHub Pages, not Connect. |
| `00-master.R`, `R/`, `README.md` | Build-time scaffolding. |

The `appFiles` argument on `rsconnect::deployApp()` enforces this — see
the snippet under [CLI deployment](#cli-deployment) below.

## Deployment targets

Pick one — they’re not mutually exclusive but one is enough to start:

| Target | Audience | Pre-requisites | Notes |
|----|----|----|----|
| **WB internal Posit Connect** | WB staff | VPN / WB network access; an account on the Connect server. | The default for World Bank deployers. Talk to GeoPov or your IT contact for an account. |
| **WB external (public-facing) Posit Connect** | Public | Approval from the relevant WB programme; account on the public Connect server. | Use when the dashboard is meant to be cited / linked from public WB material. |
| **shinyapps.io** | Public | A free or paid shinyapps.io account. | Fast path for prototypes, demos, and small public PTI apps. The free tier is enough to start. |

For internal Posit Connect, the canonical URL today is
`https://w0lxopshyprdap01.worldbank.org/` (subject to change — confirm
with GeoPov).

## Permissions setup

Before you can deploy you need:

1.  **An account on the target server.** For WB Posit Connect, contact
    the [GeoPov team](mailto:geopov@worldbank.org) (or equivalent for
    your programme) and request publisher access. shinyapps.io is
    self-service via [shinyapps.io](https://www.shinyapps.io/).
2.  **VPN connectivity** (WB internal Connect only). Without VPN the
    publish call can’t reach the server.
3.  **An API key or auth token** loaded into your IDE. RStudio prompts
    for this the first time you click Publish; the CLI flow uses
    `rsconnect::setAccountInfo()` once per server.

In RStudio: `Tools → Global Options → Publishing → Connect → Add` and
follow the prompts.

In Positron: open the command palette (`Cmd/Ctrl + Shift + P`) →
`Connect: Add Server` and follow the prompts.

## CLI deployment

The boilerplate from
[`06-deploy.R`](https://github.com/worldbank/devPTIpack/blob/main/inst/template_pti/06-deploy.R),
with the comment markers removed:

``` r

#| eval: false
rsconnect::deployApp(
  appDir   = ".",
  appFiles = c("app.R", "landing-page.md", "app-data"),
  appName  = basename(getwd()),
  server   = NULL                       # set to your Connect server, e.g. "w0lxopshyprdap01.worldbank.org"
)
```

Run from the project root. `appFiles` is the deploy whitelist — `app.R`,
`landing-page.md`, and the entire `app-data/` directory. Anything
outside that list is ignored even if it sits next to `app.R`.

`server = NULL` defaults to your single configured Connect server. If
you have multiple servers registered (e.g. WB internal + shinyapps.io),
pass the URL or short name explicitly.

## UI deployment

Both major IDEs ship a one-click publish flow that wraps `deployApp()`
under the hood:

### RStudio

1.  Open `app.R` in the editor.
2.  Click the **blue Publish button** in the top-right of the editor
    pane.
3.  In the dialog: pick the destination Connect server, confirm the file
    list (it should auto-select `app.R`, `landing-page.md`, `app-data/`
    from the manifest).
4.  Click **Publish**.

The first publish to a new server prompts for credentials (RStudio
caches them after).

### Positron

1.  Open `app.R`.
2.  Command palette (`Cmd/Ctrl + Shift + P`) → **Connect: Publish**.
3.  Same dialog flow as RStudio.

## Post-deployment

Once Posit Connect confirms the deploy, two more things to set up before
sharing the URL:

1.  **Viewer permissions.** On the Connect dashboard for your app:
    **Access → Sharing → Add user/group**. The default for new apps is
    “Owner only” — colleagues will hit a 403 until you grant them
    access. For public dashboards, set **Anyone, no login required**.
2.  **Resource limits** *(optional)*. The default Connect runtime
    settings are usually fine; only revisit if your app is unresponsive
    under realistic load.
3.  **Custom URL** *(optional)*. Connect lets you pick a friendlier URL
    slug than the default UUID. Set it under **Settings → Vanity URL**
    on your app’s Connect page.

After the first deploy, every subsequent `deployApp()` call updates the
existing app at the same URL — there’s no need to delete-and-republish.

## Optional: publish `docs/` to GitHub Pages

If `00-master.R` rendered the Quarto trail under `docs/` (the
orchestrator can do this for any documented repo), you can publish it as
a project landing page on GitHub Pages — separately from the Connect
deploy:

1.  Push the `docs/` folder to a branch (commonly `main`).
2.  In your GitHub repo: **Settings → Pages → Source = `main` /
    `/docs`**.
3.  GitHub serves the built site at `https://<org>.github.io/<repo>/`.

This is purely optional and orthogonal to the Connect deploy — `docs/`
is excluded from the Connect bundle by design.

## Where to ask for help

- **GeoPov SharePoint** — internal documentation, account requests,
  server URLs (search “Posit Connect” / “PTI dashboard” on the GeoPov
  SharePoint).
- **GitHub Issues** — open an issue at
  <https://github.com/worldbank/devPTIpack/issues> for package bugs,
  missing features, or documentation gaps.
- **Posit Connect docs** — <https://docs.posit.co/connect/> covers
  everything Connect-side (auth, scaling, scheduled jobs).
- **shinyapps.io docs** — <https://docs.posit.co/shinyapps.io/> covers
  the public-cloud target.

## Done

Your PTI app is live. To iterate:

1.  Edit your raw inputs under `sample-data/` (or replace them with new
    official data).
2.  Re-run `source("00-master.R")` — Steps 1, 3, 5 regenerate
    `app-data/`.
3.  Re-run `rsconnect::deployApp(...)` (or hit Publish) — the existing
    Connect URL updates in place.
