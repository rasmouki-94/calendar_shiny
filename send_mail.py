"""
send_mail.py — Notification email via Gmail SMTP (SSL).
Appelé depuis R via reticulate.
"""

import os
import smtplib
from email.message import EmailMessage
from dotenv import load_dotenv

# Charge les variables du fichier .env
load_dotenv()


def send_notification_email(nom: str, entreprise: str, email: str, besoin: str) -> None:
    """
    Envoie un email de notification avec les infos du prospect.

    Args:
        nom: Nom complet du prospect.
        entreprise: Nom de l'entreprise.
        email: Adresse email du prospect.
        besoin: Besoin sélectionné dans le formulaire.
    """
    gmail_user = os.getenv("GMAIL_USER")
    gmail_password = os.getenv("GMAIL_APP_PASSWORD")

    if not gmail_user or not gmail_password:
        raise EnvironmentError(
            "Les variables GMAIL_USER et GMAIL_APP_PASSWORD doivent être "
            "définies dans le fichier .env"
        )

    # Construction du message
    msg = EmailMessage()
    msg["Subject"] = f"Nouveau prospect — {nom} ({entreprise})"
    msg["From"] = gmail_user
    msg["To"] = gmail_user  # Notification envoyée à toi-même

    body = f"""\
Nouveau prospect qualifié via la landing page.

--------------------------------------
Nom        : {nom}
Entreprise : {entreprise}
Email      : {email}
Besoin     : {besoin}
--------------------------------------

Ce prospect va maintenant choisir un créneau sur Cal.com.
"""
    msg.set_content(body)

    # Envoi via SMTP_SSL (port 465)
    with smtplib.SMTP_SSL("smtp.gmail.com", 465) as smtp:
        smtp.login(gmail_user, gmail_password)
        smtp.send_message(msg)

    print(f"[send_mail] Email envoyé pour {nom} ({email})")
