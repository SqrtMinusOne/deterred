import base64
import io

__all__ = ['fig_to_b64']

def fig_to_b64(fig):
    buf = io.BytesIO()
    fig.tight_layout()
    fig.savefig(buf, format='png')
    img = base64.b64encode(buf.getvalue()).decode()
    return img
