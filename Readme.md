# Research Methods HS26 — Uebungsunterlagen

Quarto-Book mit Demos und Übungen für den Kurs Research Methods (Umweltingenieurwesen, ZHAW).

## Rendern und Publizieren

### Hauptseite

```bash
quarto render        # rendert nach _site/
quarto publish       # publiziert Hauptseite (Destination in _publish.yml)
```

### Entwicklungsvorschau (Profil `dev`)

Das Profil `dev` (`_quarto-dev.yml`) rendert nur die Prepro-4-Inhalte nach `_dev/` und eignet sich für Vorschauen vor der Aufschaltung auf die Hauptseite.

```bash
quarto render --profile dev               # rendert nach _dev/
quarto preview --profile dev              # lokale Vorschau

# Publizieren (erstes Mal: Quarto fragt nach Destination und URL)
quarto publish quarto-pub --profile dev

# Erneut publizieren ohne Re-Render
quarto publish posit-cloud --no-render --profile dev
```

Die Publish-Konfiguration (URL, Site-ID) wird automatisch in `_publish.yml` gespeichert.

Aktuelle Vorschau-URL: https://connect.posit.cloud/ratnanil/content/01a116ee-12a8-9ed3-f523-667a943814c0
