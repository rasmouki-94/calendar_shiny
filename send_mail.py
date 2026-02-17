"""
send_mail.py — Notification email via Gmail SMTP (SSL).
Appelé depuis R via reticulate.

Variables d'environnement attendues (Posit Cloud > Settings > Environment Variables) :
  - GMAIL_USER         : Adresse Gmail utilisée pour l'envoi
  - GMAIL_APP_PASSWORD : Mot de passe d'application Google (16 caractères)
  - EMAIL_TO           : Adresse de réception de la notification
"""

import os
import smtplib
from email.message import EmailMessage


def send_notification_email(nom: str, entreprise: str, email: str) -> None:
    gmail_user = os.getenv("GMAIL_USER")
    gmail_password = os.getenv("GMAIL_APP_PASSWORD")
    email_to = os.getenv("EMAIL_TO")

    if not gmail_user or not gmail_password or not email_to:
        raise EnvironmentError(
            "Les variables GMAIL_USER, GMAIL_APP_PASSWORD et EMAIL_TO "
            "doivent être définies (Posit Cloud > Settings > Environment Variables)."
        )

    msg = EmailMessage()
    msg["Subject"] = f"Nouveau prospect — {nom} ({entreprise})"
    msg["From"] = gmail_user
    msg["To"] = email_to

    body = f"""\
Nouveau prospect qualifié via la landing page.

--------------------------------------
Nom        : {nom}
Entreprise : {entreprise}
Email      : {email}
--------------------------------------

Ce prospect va maintenant choisir un créneau sur Cal.com.
"""
    msg.set_content(body)

    with smtplib.SMTP_SSL("smtp.gmail.com", 465) as smtp:
        smtp.login(gmail_user, gmail_password)
        smtp.send_message(msg)

    print(f"[send_mail] Email envoyé pour {nom} ({email}) -> {email_to}")
