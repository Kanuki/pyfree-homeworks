"""
Веб-интерфейс для помощника-скачивателя: вводишь название документа на
странице в браузере, жмёшь «Скачать» — сервер сам ищет документ и
скачивает файл, чьё имя совпадает с запросом.

Перед первым запуском один раз выполнить:
    pip install playwright flask
    playwright install chromium

Запуск:
    python doc_downloader_web.py
Затем открыть в браузере: http://127.0.0.1:5000
"""
from flask import Flask, render_template_string, request

from doc_downloader_hw6 import find_and_download

app = Flask(__name__)

PAGE = """
<!doctype html>
<html lang="ru">
<head>
<meta charset="utf-8">
<title>Помощник-скачиватель</title>
<style>
  body { font-family: sans-serif; max-width: 560px; margin: 40px auto; padding: 0 16px; }
  label { display: block; margin-top: 12px; }
  input[type=text] { width: 100%; padding: 8px; box-sizing: border-box; }
  button { margin-top: 16px; padding: 8px 20px; }
  .result { margin-top: 20px; padding: 12px; border-radius: 6px; }
  .ok { background: #e6ffed; border: 1px solid #34a853; }
  .err { background: #ffe6e6; border: 1px solid #d93025; }
  .busy { color: #666; }
</style>
</head>
<body>
  <h1>Помощник-скачиватель</h1>
  <form method="post">
    <label>Название документа
      <input type="text" name="query" value="{{ query or '' }}" required autofocus>
    </label>
    <label>
      <input type="checkbox" name="exact" {{ 'checked' if exact else '' }}>
      Точное совпадение имени файла
    </label>
    <button type="submit">Скачать</button>
  </form>
  {% if result %}
    <div class="result {{ 'ok' if result.ok else 'err' }}">{{ result.message }}</div>
  {% endif %}
</body>
</html>
"""


@app.route('/', methods=['GET', 'POST'])
def index():
    query = None
    exact = False
    result = None
    if request.method == 'POST':
        query = request.form.get('query', '').strip()
        exact = bool(request.form.get('exact'))
        if query:
            try:
                path = find_and_download(query, exact=exact, headless=True)
            except Exception as e:
                result = {'ok': False, 'message': f'Ошибка: {e}'}
            else:
                if path:
                    result = {'ok': True, 'message': f'Скачано: {path}'}
                else:
                    result = {'ok': False, 'message': f'Документ по запросу «{query}» не найден.'}
    return render_template_string(PAGE, query=query, exact=exact, result=result)


if __name__ == '__main__':
    app.run(debug=False, port=5000)
