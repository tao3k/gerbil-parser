# pinned HCL native syntax fixture
resource "service" "web" {
  name = app.name
  ports = [80, 443]
  metadata = {
    owner = team.platform
    tier = "frontend"
  }
  health { enabled = true }
}
