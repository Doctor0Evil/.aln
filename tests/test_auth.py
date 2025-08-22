```python
from fastapi.testclient import TestClient
from src.api.auth import app

client = TestClient(app)

def test_login_and_access_secure_data():
    response = client.post("/token", data={"username": "admin", "password": "secret"})
    assert response.status_code == 200
    token = response.json()["access_token"]

    response2 = client.get("/secure-data", headers={"Authorization": f"Bearer {token}"})
    assert response2.status_code == 200
    assert "secure_content" in response2.json()

def test_invalid_login():
    response = client.post("/token", data={"username": "wrong", "password": "bad"})
    assert response.status_code == 401
```
