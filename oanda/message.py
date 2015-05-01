import smtplib
from email.mime.text import MIMEText

def send(content = ''):
    fromaddr = 'gavinlau.robot@gmail.com'
    toaddr  = 'trigger@recipe.ifttt.com'
    msg = MIMEText('')
    msg['Subject'] = '%s #oanda' % (content,)
    msg['From'] = fromaddr
    msg['To'] = toaddr
    username = 'gavinlau.robot'
    password = 'glave1234567890'

    server = smtplib.SMTP('smtp.gmail.com', 587)
    server.starttls()
    server.login(username, password)
    server.sendmail(fromaddr, toaddr, msg.as_string())
    server.quit()
    return True